#####################################################################
# BPM 14 - CrowdFlower unit execution time calculation
# add file crowdflower_secret.R with:
# CROWDFLOWER_SECRET_KEY <- "YOUR_SECRET_KEY"
#####################################################################

source("crowdflower_tasks_performance.R")

#####################################################################
# STREAMING
# 2015-03-19
# 14:00, 16:00, 20:00
#####################################################################
to_download = FALSE

batch1 <- processBatch(c(704588,704586,704587), "12:00", "Streaming",40,to_download)
batch2 <- processBatch(c(704621,704623,704622), "16:00", "Streaming",40,to_download)
batch3 <- processBatch(c(704730,704733,704731), "20:00", "Streaming",40,to_download)

batches_with_streaming <- rbind(batch1, batch2, batch3)
#####################################################################
# NON-STREAMING
# 2015-03-20
# 14:00, 16:00, 20:00
#####################################################################

batch4 <- processBatch(c(705168,705170,705169), "12:00", "No streaming",40,to_download)
batch5 <- processBatch(c(705226,705228,705227), "16:00", "No streaming",40,to_download)
batch6 <- processBatch(c(705241,705243,705242), "20:00", "No streaming",40,to_download)

batches_no_streaming <- rbind(batch4, batch5, batch6)
#####################################################################

batches_all <- rbind(batches_with_streaming,batches_no_streaming)

#####################################################################
# PLOT GENERATION
#####################################################################

createPlot(batches_all,"plots/streaming_comparison_1_5hour_limit_bw.pdf",135, 16,5, task~streaming )
