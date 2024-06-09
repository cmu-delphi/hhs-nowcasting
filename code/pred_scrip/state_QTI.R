source("assets_QTI/quantTrack.r")


frames = quantileTrack_Baseline(c(0.2, 0.4))
saveRDS(frames, file = "../../predictions/quanTrack_Baseline.rds")

