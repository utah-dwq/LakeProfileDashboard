### Utah DWQ Lake Profile Dashboard
### Jake Vander Laan, Utah DWQ, jvander@utah.gov
### Version 1.0, Feb 6 2019

library(wqTools)

ui <-fluidPage(
	
	# Header
	headerPanel(title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85)),
		windowTitle="Lake Profile Dashboard"),
		
	# Title
	titlePanel("",
		tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"),
		tags$title("Lake Profile Dashboard"))
		#tags$a(href="www.rstudio.com", "Click here!")
	),
	
	# Input widgets
	fluidRow(
		conditionalPanel(condition="input.tabs!='User guide'",
			column(4,h4("Click a site"),leaflet::leafletOutput("map", height="600px"))
		),
		conditionalPanel(condition="input.tabs=='User guide'",
			column(4)
		),
		column(8,tabsetPanel(id="tabs",
			tabPanel("Time series",
				fluidRow(column(8,
					uiOutput("date_slider"),
					radioButtons("ts_plot_type","Plot type:", choices=c("Heatmap", "Habitable width", "Water column exceedances"), inline=T),
					conditionalPanel(condition="input.ts_plot_type=='Heatmap'",
						selectInput("heatmap_param",label="Heatmap parameter:",choices=c("Dissolved oxygen","Temperature","pH", "DO/temp lens"))
					),
					checkboxInput("show_dates", label="Show all profile dates"),
					conditionalPanel(condition="input.ts_plot_type=='Heatmap'",
						plotOutput("heatmap")
					),
					conditionalPanel(condition="input.ts_plot_type=='Habitable width'",
						plotOutput("hab_width")
					),
					conditionalPanel(condition="input.ts_plot_type=='Water column exceedances'",
						plotOutput("pct_exc")
					)
				))
			),
			tabPanel("Individual profiles",
				fluidRow(
					column(4, uiOutput("date_select"))
				),
				fluidRow(
					column(4,h4("Profile plot"),plotOutput("ind_prof_plot", height="500px")),
					column(8,h4("Profile data"),div(dataTableOutput("profile_table"), style = "font-size:80%"))
				)
			),
			tabPanel("User guide", 
				fluidRow(
					column(8,
						includeMarkdown('./user_guide/user_guide.rmd')
					)
				)
			)
		))
	)
)

server <- function(input, output, session){

	# Load data
	load("./data/assessed_profs.rdata")
	
	# Subset polygons to lake polygons
	data(au_poly)
	lake_aus=au_poly[au_poly$AU_Type=="Reservoir/Lake",]
	
	# Extract site locations
	prof_sites=unique(assessed_profs$profile_asmnts_mlid_param[,c("ASSESS_ID","AU_NAME","R317Descrp","BEN_CLASS","IR_MLID","IR_MLNAME","IR_Lat","IR_Long")])
	prof_sites$MonitoringLocationTypeName="Lake/Reservoir"
	prof_sites=plyr::rename(prof_sites, c("IR_Lat"="LatitudeMeasure", "IR_Long"="LongitudeMeasure","IR_MLID"="MonitoringLocationIdentifier","IR_MLNAME"="MonitoringLocationName"))
		
	# Extract profiles long
	profiles_long=assessed_profs$profiles_long
	profiles_long$MonitoringLocationIdentifier=profiles_long$IR_MLID
	profiles_long=unique(profiles_long[,c("DataLoggerLine","ActivityIdentifier","ActivityStartDate","R3172ParameterName","IR_Value","IR_Unit","NumericCriterion","MonitoringLocationIdentifier")])
	profiles_long$ActivityStartDate=as.Date(profiles_long$ActivityStartDate,format='%Y-%m-%d')

	# Remove profiles where depths are not provided
	depths=profiles_long[profiles_long$R3172ParameterName=="Profile depth",]
	depth_actids=unique(depths$ActivityIdentifier)
	profiles_long=profiles_long[profiles_long$ActivityIdentifier %in% depth_actids,]
	
	# Remove any sites that do not produce any valid profiles
	prof_sites=prof_sites[prof_sites$MonitoringLocationIdentifier %in% profiles_long$MonitoringLocationIdentifier,]
	
	# Extract profiles wide
	profiles_wide=assessed_profs$profiles_wide
	profiles_wide=profiles_wide[profiles_wide$ActivityIdentifier %in% profiles_long$ActivityIdentifier,]
	profiles_wide$ActivityStartDate=as.Date(profiles_wide$ActivityStartDate,format='%Y-%m-%d')
	
	# Calc max depth for each profile
	max_depth=aggregate(Depth_m~ActivityIdentifier,data=profiles_wide, FUN='max', na.rm=T)
	names(max_depth)[names(max_depth)=="Depth_m"]="max_depth_m"
	
	# Extract individual profile assessments
	ind_prof_asmnts=assessed_profs$profile_asmnts_individual
	ind_prof_asmnts=ind_prof_asmnts[ind_prof_asmnts$ActivityIdentifier %in% profiles_long$ActivityIdentifier,]
	ind_prof_asmnts$ActivityStartDate=as.Date(ind_prof_asmnts$ActivityStartDate,format='%Y-%m-%d')
	ind_prof_asmnts=merge(ind_prof_asmnts,max_depth,all.x=T)
	ind_prof_asmnts=within(ind_prof_asmnts,{
		ph_pct_exc=pH_exc_cnt/samp_count*100
		temp_pct_exc=temp_exc_cnt/samp_count*100
		do_pct_exc=do_exc_cnt/samp_count*100
	})
	
	# Empty reactive values object
	reactive_objects=reactiveValues()
			
	# Resources for returning site info on click:
	## https://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
	## https://stackoverflow.com/questions/42613984/how-to-implement-inputmap-marker-click-correctly?noredirect=1&lq=1
	
	
	# Select map set up
    map = leaflet::createLeafletMap(session, 'map')

    session$onFlushed(once = T, function() {
		output$map <- leaflet::renderLeaflet({
			buildMap(sites=prof_sites, plot_polys=TRUE, au_poly=lake_aus)
		})
    })
	
	# Map marker click
	observe({
		req(profiles_long)
		click <- input$map_marker_click
		if (is.null(click)){return()}
		siteid=click$id
		reactive_objects$sel_mlid=siteid
		reactive_objects$sel_profiles=profiles_long[profiles_long$MonitoringLocationIdentifier==siteid,]
		profile_dates=unique(reactive_objects$sel_profiles$ActivityStartDate)
		profile_dates=profile_dates[order(profile_dates)]
		reactive_objects$profile_dates=profile_dates
	})
		
	# Profile date selection
	output$date_select <- renderUI({
		req(reactive_objects$profile_dates)
		selectInput("date_select", "Profile date:", reactive_objects$profile_dates)
	})
	output$date_slider <- renderUI({
		req(reactive_objects$profile_dates)
		date_min=min(reactive_objects$profile_dates)
		date_max=max(reactive_objects$profile_dates)
		sliderInput("date_slider", "Date range:", min=date_min, max=date_max, value=c(date_min,date_max))
	})

	# Generate selected aid
	observe({
		req(input$date_select)
		reactive_objects$selectedActID=reactive_objects$sel_profiles[reactive_objects$sel_profiles$ActivityStartDate==input$date_select,"ActivityIdentifier"][1]
	})

	# Profile plot output
	output$ind_prof_plot=renderPlot({
		req(reactive_objects$sel_profiles,reactive_objects$selectedActID)
		one_profile=reactive_objects$sel_profiles[reactive_objects$sel_profiles$ActivityIdentifier==reactive_objects$selectedActID,]
		
		do_crit=one_profile[one_profile$R3172ParameterName=="Minimum Dissolved Oxygen","NumericCriterion"][1]
		temp_crit=one_profile[one_profile$R3172ParameterName=="Temperature, water","NumericCriterion"][1]

		one_profile=unique(one_profile[,c("DataLoggerLine","ActivityIdentifier","ActivityStartDate","R3172ParameterName","IR_Value","IR_Unit","MonitoringLocationIdentifier")])
		

		profilePlot(one_profile, parameter = "R3172ParameterName",
			units = "IR_Unit",
			depth = "Profile depth", do = "Minimum Dissolved Oxygen",
			temp = "Temperature, water", pH = "pH",
			value_var = "IR_Value", line_no = "DataLoggerLine",
			pH_crit=c(6.5,9), do_crit=do_crit, temp_crit=temp_crit)
		box()
	})
	
	# Data table output
	observe({
		req(reactive_objects$selectedActID)
		table_data=profiles_wide[profiles_wide$ActivityIdentifier==reactive_objects$selectedActID,c("IR_MLID","ActivityStartDate","Depth_m","DO_mgL","pH","Temp_degC")]
		reactive_objects$table_data=table_data[order(table_data$Depth_m),]
	})
	output$profile_table=renderDataTable({
		req(reactive_objects$table_data)
		reactive_objects$table_data
	},
		options = list(scrollY = '500px', paging = FALSE, scrollX = TRUE, searching=F)
	)
	
	# Extract profile assessments & profiles_wide for selected site
	observe({
		req(reactive_objects$sel_mlid,input$date_slider)
		selected_prof_asmnts=ind_prof_asmnts[
			ind_prof_asmnts$IR_MLID == reactive_objects$sel_mlid &
			ind_prof_asmnts$ActivityStartDate>=input$date_slider[1] &
			ind_prof_asmnts$ActivityStartDate<=input$date_slider[2]
		,]
		selected_prof_asmnts=selected_prof_asmnts[order(selected_prof_asmnts$ActivityStartDate),]
		reactive_objects$selected_prof_asmnts=selected_prof_asmnts	
		
		reactive_objects$sel_profs_wide=profiles_wide[
			profiles_wide$IR_MLID == reactive_objects$sel_mlid &
			profiles_wide$ActivityStartDate>=input$date_slider[1] &
			profiles_wide$ActivityStartDate<=input$date_slider[2]
		,]
		})
	
	# Hab width plot output
	output$hab_width=renderPlot({
		req(reactive_objects$selected_prof_asmnts)
		if(dim(reactive_objects$selected_prof_asmnts)[1]>0){
			par(mar=c(7.1,5.1,7.1,2.1))
			plot(max_hab_width~ActivityStartDate, data=reactive_objects$selected_prof_asmnts, pch=NA, cex=1.5, ylab="Width (m)", xlab="", cex.axis=1.25, cex.lab=1.5, xaxt='n',
				ylim=c(0,max(reactive_objects$selected_prof_asmnts$max_depth_m,na.rm=T))
			)
			abline(h=3,lty=3,lwd=2,col="red")
			if(input$show_dates){
				axis(1, at=unique(reactive_objects$selected_prof_asmnts$ActivityStartDate), labels=unique(as.Date(reactive_objects$selected_prof_asmnts$ActivityStartDate)), par(las=2))
			}else{
				axis.Date(1, reactive_objects$selected_prof_asmnts$ActivityStartDate)
			}
			points(max_depth_m~ActivityStartDate, data=reactive_objects$selected_prof_asmnts, type='l',lty=2,lwd=2,col="blue")
			points(max_hab_width~ActivityStartDate, data=reactive_objects$selected_prof_asmnts, type='b', pch=21, cex=1.5, bg="grey", cex.axis=1.25, cex.lab=1.5)
			par(xpd=TRUE)
				legend("topleft", inset=c(0.05,-0.3), bty='n', pch=c(NA,21),pt.bg=c(NA,'grey'),lty=c(2,1),col=c("blue","black"),lwd=c(2,1),cex=1.5, legend=c("Max depth","DO/temp habitat"))
			par(xpd=FALSE)
		}
	})


	# pct exceedance plot
	output$pct_exc=renderPlot({
		req(reactive_objects$selected_prof_asmnts)
		if(dim(reactive_objects$selected_prof_asmnts)[1]>0){
			ymax=max(5,max(max(reactive_objects$selected_prof_asmnts$do_pct_exc, na.rm=T),max(reactive_objects$selected_prof_asmnts$temp_pct_exc, na.rm=T),max(reactive_objects$selected_prof_asmnts$ph_pct_exc, na.rm=T))*1.1)
			
			par(mar=c(7.1,5.1,7.1,2.1))
	
			plot(do_pct_exc~ActivityStartDate, data=reactive_objects$selected_prof_asmnts,ylim=c(0,ymax), pch=24, bg="deepskyblue3", type='b', ylab="% exceedance", cex=1.5, xlab="", xaxt='n')
			points(temp_pct_exc~ActivityStartDate, data=reactive_objects$selected_prof_asmnts, pch=21, bg="orange", type='b', cex=1.5)
			points(ph_pct_exc~ActivityStartDate, data=reactive_objects$selected_prof_asmnts, pch=22, bg="green", type='b', cex=1.5)
			if(input$show_dates){
				axis(1, at=unique(reactive_objects$selected_prof_asmnts$ActivityStartDate), labels=unique(as.Date(reactive_objects$selected_prof_asmnts$ActivityStartDate)), par(las=2))
			}else{
				axis.Date(1, reactive_objects$selected_prof_asmnts$ActivityStartDate)
			}
			par(xpd=TRUE)
			legend("topleft", inset=c(0.05,-0.3), bty='n',horiz=T,
				legend=c("Dissolved oxygen","Temperature","pH"),
				pch=c(24,21,22), pt.bg=c("deepskyblue3","orange","green"), cex=1.5)
			par(xpd=FALSE)
		}
	})

	# Profile heatmap plot
	output$heatmap=renderPlot({
		req(reactive_objects$sel_profs_wide, reactive_objects$sel_profiles)
		if(dim(reactive_objects$sel_profs_wide)[1]>0){
			if(length(unique(reactive_objects$sel_profs_wide$ActivityStartDate))==1){
				plot.new()
				text(0.5,0.5,"Only one profile date available. Cannot interpolate.")
				box()
			}else{
				# Define heatmap inputs based on selected parameter
				if(input$heatmap_param=="Dissolved oxygen"){
					name="Minimum Dissolved Oxygen"
					parameter="DO_mgL"
					param_units="mg/L"
					param_lab="Dissolved oxygen"
				}
				if(input$heatmap_param=="pH"){
					name="pH"
					parameter="pH"
					param_units=""
					param_lab="pH"
				}
				if(input$heatmap_param=="Temperature"){
					name="Temperature, water"
					parameter="Temp_degC"
					param_units="deg C"
					param_lab="Temperature"
				}
				if(input$heatmap_param=="DO/temp lens"){
					parameter="do_temp_exc"
					param_units="deg C"
					param_lab="DO/temp exc."
				}
				# Define criteria
				if(input$heatmap_param!="DO/temp lens"){
					criteria=unique(reactive_objects$sel_profiles[reactive_objects$sel_profiles$R3172ParameterName==name,"NumericCriterion"])
				}else{criteria=1}
				# heat map
				if(input$show_dates){show_dates=TRUE}else{show_dates=FALSE}
				profileHeatMap(reactive_objects$sel_profs_wide,parameter=parameter,param_units=param_units,param_lab=param_lab,depth="Depth_m",depth_units="m",criteria=criteria,show_dates=show_dates)
		}
		}
	})
	

}

## run app 
shinyApp(ui = ui, server = server)
