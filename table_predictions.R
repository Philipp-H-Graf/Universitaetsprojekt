
predictions <- matrix(nrow = length((inflation_table)), ncol = 9)

colnames(predictions) <- c(paste0("Q", 1:4, "_22"), paste0("Q", 1:4, "_23"), "Q1_24")
rownames(predictions) <- names(inflation_table)

#for(j in length((inflation_table))){
#    predictions[j, ] <- inflation_table[[j]]$Inflation[49:57]
#}

predictions[1, ] <- inflation_table[[1]]$Inflation[49:57]
predictions[2, ] <- inflation_table[[2]]$Inflation[49:57]
predictions[3, ] <- inflation_table[[3]]$Inflation[49:57]
predictions[4, ] <- inflation_table[[4]]$Inflation[49:57]
predictions[5, ] <- inflation_table[[5]]$Inflation[49:57]
predictions[6, ] <- inflation_table[[6]]$Inflation[49:57]
predictions[7, ] <- inflation_table[[7]]$Inflation[49:57]
predictions[8, ] <- inflation_table[[8]]$Inflation[49:57]
predictions[9, ] <- inflation_table[[9]]$Inflation[49:57]
predictions[10, ] <- inflation_table[[10]]$Inflation[49:57]
predictions[11, ] <- inflation_table[[11]]$Inflation[49:57]
predictions[12, ] <- inflation_table[[12]]$Inflation[49:57]
predictions[13, ] <- inflation_table[[13]]$Inflation[49:57]
predictions[14, ] <- inflation_table[[14]]$Inflation[49:57]
predictions[15, ] <- inflation_table[[15]]$Inflation[49:57]
predictions[16, ] <- inflation_table[[16]]$Inflation[49:57]
predictions[17, ] <- inflation_table[[17]]$Inflation[49:57]
predictions[18, ] <- inflation_table[[18]]$Inflation[49:57]
predictions[19, ] <- inflation_table[[19]]$Inflation[49:57]

save(predictions, file = "predictions.RData")

DT::datatable(predictions)




