```{r}
library(tidyverse)
library(IRanges)

left_join_overlaps <- 
    function(x,y, join_column){
        x_range <- 
            IRanges(
                start = 
                    pull(x, start),
                end = 
                    pull(x, end)
            )
        y_range <- 
            IRanges(
                start = 
                    pull(y, start),
                end = 
                    pull(y, end)
            )
        
        overlap_table <- 
            IRanges::findOverlaps(
                x_range, y_range, type = "any"
            )
        
        x_overlaps <- 
            queryHits(overlap_table)
        
        y_overlaps <- 
            subjectHits(overlap_table)
        

        
        y_joining <- 
            y %>%
            dplyr::slice(y_overlaps)
        
        x_joined <- 
            left_join(x, y_joining, by = join_column)
            
        return(x_joined)
        
    }



```
