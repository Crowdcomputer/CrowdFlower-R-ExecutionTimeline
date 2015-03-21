#####################################################################
# BPM 14 - CrowdFlower unit execution time calculation
#####################################################################

# download required libraries
# install.packages('downloader')
# install.packages('iterators')
# install.packages('lubridate')

library(downloader)
library(lubridate)
library(ggplot2)
library(scales)

dumb_start_time <- as.POSIXct("03/19/2015 00:00:00", format='%m/%d/%Y %H:%M:%S')

createPlot <- function(data, filename, binwidth, width, height, faceting){
	hist_plot <- ggplot(data, aes(x=X_relative_time,y=..count.., fill = batch))
	# plot histogram
	hist_plot <- hist_plot + geom_histogram(binwidth=binwidth, position="identity", alpha = 0.6, color="black")
	# plot labels
	hist_plot <- hist_plot + xlab("Time") + ylab("Tasks completed") #+ ggtitle("Streaming") 
	hist_plot <- hist_plot + scale_x_datetime(breaks = date_breaks("20 min"), minor_breaks = date_breaks("10 min"),labels = date_format("%H:%M"))
	# split into facets
	hist_plot <- hist_plot + facet_grid(faceting)
	# font size, theme settings
	hist_plot <- hist_plot + theme(text = element_text(size=12, color = "black"), axis.text = element_text(size=12, color = "black"), title = element_text(size=16),strip.text = element_text(size = 16), strip.text.y = element_text(angle = 0),axis.title.y=element_text(vjust=1.5)) 
	# task end vertical lines
	hist_plot <- hist_plot+ geom_vline(aes(xintercept=fin, colour = batch), data)
	# save the plot into the file
	ggsave(hist_plot, file=filename, width=width, height=height)
}
getJobResults <- function(job_id, title, batch, streaming, subtract, scope, download = F){

	# (comment if is already downloaded)download the latest zip file with full results for the target job
	if (download){
		source("crowdflower_secret.R")
		download(paste("https://api.crowdflower.com/v1/jobs/",job_id,".csv?type=full&key=",CROWDFLOWER_SECRET_KEY, sep = ""), mode = "wb", destfile = paste("output/",job_id,".zip", sep=""))
	}
	# read the csv from the zip file
	data <- read.table(unz(paste("output/",job_id,".zip", sep=""), paste("f",job_id,".csv", sep="")), header=T, sep=",",quote = "\"",comment.char = "")

	# parse string/factor columns into date format
	data$X_created_at <- mdy_hms(data$X_created_at)
	# remove outliers
	# ------------------------------------------
	if(scope != 40) {
		data <- data[with(data, order(X_created_at)), ]
		if (title == "1. Transcribe"){
			data <- data[1:scope,]
		}
		if (title == "2. Check and fix"){
			data <- data[1:(scope/2),]
		}
		if (title == "3. Classify receipt"){
			data <- data[1:(scope/4),]
		}
	}
	# ------------------------------------------
	if (subtract){
		data$min_created_at <- subtract
	}else{
		data$min_created_at <- min(data$X_created_at)
	}

	data$X_relative_time <- dumb_start_time + (data$X_created_at - data$min_created_at)
	data$X_started_at <- mdy_hms(data$X_started_at)
	
	data["task"] <- title
	end <- max(data$X_relative_time)
	data["fin"] <- as.numeric(end)
	data["batch"] <- batch
	data["streaming"] <- streaming
	
	subset(data, select=c("task","batch","streaming", "fin", "X_created_at","X_started_at","X_relative_time"))
}

processBatch <- function(batch_tasks, batch_name, condition, scope, download = F){
	task_names <- c("1. Transcribe","2. Check and fix","3. Classify receipt")
	
	if (condition == "Streaming"){
		task1 <- getJobResults(batch_tasks[1], task_names[1], batch_name, condition, F,scope, download)
		task2 <- getJobResults(batch_tasks[2], task_names[2], batch_name, condition, min(task1$X_created_at),scope, download)
		task3 <- getJobResults(batch_tasks[3], task_names[3], batch_name, condition, min(task1$X_created_at),scope, download)
	}else{
		task1 <- getJobResults(batch_tasks[1], task_names[1], batch_name, condition,F,scope, download)
		
		test1 <- getJobResults(batch_tasks[2], task_names[2], batch_name, condition, min(task1$X_created_at),scope, download)
		test2 <- getJobResults(batch_tasks[3], task_names[3], batch_name, condition, min(task1$X_created_at),scope, download)
		
		task2 <- getJobResults(batch_tasks[2], task_names[2], batch_name, condition, min(task1$X_created_at)+(min(test1$X_created_at)-max(task1$X_created_at)),scope, download)
		task3 <- getJobResults(batch_tasks[3], task_names[3], batch_name, condition, min(task1$X_created_at)+(min(test2$X_created_at)-max(task1$X_created_at)),scope, download)
	}
	batch <- rbind(task1,task2,task3)
	batch
}
