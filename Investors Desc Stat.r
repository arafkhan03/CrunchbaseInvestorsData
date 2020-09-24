colMeans(is.na(investors))
format(colMeans(is.na(investors)), scientific = FALSE)

# Investors Data Descriptive Stats

# 1.0 Data Preparation and Library Loading
library(skimr)
library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)

        ## Summary
dim(investors)
skim(investors)

        ## Only Keep Columns that have NAs Less than 75%
investors_NA <- as.data.frame(
        round(colMeans(is.na(investors)), 2)
)
sum(investors_NA$`round(colMeans(is.na(investors_NA)), 2)` < 0.75)
investors_WNA <- investors[, investors_NA$`round(colMeans(is.na(investors)), 2)` < 0.75]

        ## Summary
skim(investors_WNA)






# 2.0 Number of Investors by Regions

        ## Summarize No. of Companies by HQ's Country
region_count_investors <- investors_WNA %>%
        dplyr::group_by(Regions, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Regions = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Sort DF
region_count_investors <-  arrange(region_count_investors, desc(Count_by_Regions))

        ## Add Percentage
region_count_investors$Percentage <- 
        as.numeric(format(region_count_investors$Count_by_Regions/
                       sum(region_count_investors$Count_by_Regions), 
               scientific = FALSE, digits = 2))

        ## Take Top 20
top20_region_count_investors <- region_count_investors[1:20,]
sum(top20_region_count_investors$Percentage)

        ## Plot Number of Investors by Regions
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

                ### Generate the layout. This function return a dataframe with one line per bubble. 
                ### It gives its center (x and y) and its radius, proportional of the value
regions_packing_investors <- circleProgressiveLayout(top20_region_count_investors$Count_by_Regions, 
                                   sizetype='area')

                ### We can add these packing information to the initial data frame
data_investors_regions_circlepack <- cbind(top20_region_count_investors, regions_packing_investors)

                ### The next step is to go from one center + a radius to the coordinates of a circle that
                ### is drawn by a multitude of straight lines.
dat.gg_regions_investors_circle <- circleLayoutVertices(regions_packing_investors, 
                                                        npoints=50)

        ## Make the plot
HQ_Regions_Investors_Circleplot <- ggplot() + 
        
                ### Make the bubbles
        geom_polygon(data = dat.gg_regions_investors_circle, aes(x, y, group = id, 
                                                       fill=as.factor(id)), 
                     colour = "black", alpha = 0.6) +
        
                ### Add text in the center of each bubble + control its size
        geom_text(data = data_investors_regions_circlepack, aes(x, y, size=Count_by_Regions, 
                                                      label = Regions)) +
        ggtitle("Distribution of Investors by Region") +
        
                ### General theme:
        scale_size_continuous(range = c(1,4)) +
        theme_void() + 
        theme(legend.position="none") +
        coord_equal()

HQ_Regions_Investors_Circleplot






# 3.0 Number of Investors by Gender

        ## Summarize No. of Investors by Gender
gender_count_investors <- investors_WNA %>%
        dplyr::group_by(Gender, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Gender = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Arrange DF
gender_count_investors <- arrange(gender_count_investors, 
                                  desc(gender_count_investors$Count_by_Gender))

        ## Percentage of Investors by Genders
gender_count_investors$Percentage <- as.numeric(format(gender_count_investors$Count_by_Gender/
                                                           sum(gender_count_investors$Count_by_Gender), 
                                                   scientific = FALSE, digits = 2))

        ## Write Table
write.table(gender_count_investors, file = "/Users/araf03/Desktop/MS Thesis/Tables/Investors/Investors by Genders.txt",
            sep = ",")






# 4.0 Number of Investors by Number of Portfolio Organizations

        ## Cut Continuous Variable into Factors
investors_WNA$num_portfolio_org_cat <- cut(investors_WNA$`Number of Portfolio Organizations`,
                                           breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 9, 11, 
                                                      15, 20, 50, 100, 500, 1000,
                                                      5000, 10000))

        ## Summarize No. of Investors by Number of Portfolio Organizations
portfolio_count_investors <- investors_WNA %>%
        dplyr::group_by(num_portfolio_org_cat, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Portfolio = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Percentage of Investors by Number of Portfolio Organizations
portfolio_count_investors$Percentage <- as.numeric(format(portfolio_count_investors$Count_by_Portfolio/
                                                           sum(portfolio_count_investors$Count_by_Portfolio), 
                                                   scientific = FALSE, digits = 2))

        ## Plot No. of Investors by Portfolio Orgs
ylim_investors_portfolio <- c(0, 1.1*max(portfolio_count_investors$Count_by_Portfolio))
xx_investors_portfolio <- barplot(portfolio_count_investors$Count_by_Portfolio, xaxt = 'n', xlab = '', width = 0.85,
                               ylim = ylim_investors_portfolio, main = "Number of Investors by Number of Organizations in Portfolio", 
                               ylab = "Frequency")
text(x = xx_investors_portfolio, y = portfolio_count_investors$Count_by_Portfolio, 
     label = portfolio_count_investors$Count_by_Portfolio, pos = 3, cex = 0.8, col = "black")
axis(1, xx_investors_portfolio, labels=portfolio_count_investors$num_portfolio_org_cat, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 5.0 Number of Investors by Number of Investments

        ## Cut Continuous Variable into Factors
investors_WNA$num_investments_cat <- cut(investors_WNA$`Number of Investments`,
                                           breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 9, 11, 
                                                      15, 20, 50, 100, 500, 1000,
                                                      4000))

        ## Summarize No. of Investors by Number of Investments
investments_count_investors <- investors_WNA %>%
        dplyr::group_by(num_investments_cat, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Investments = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Percentage of Investors by Number of Investments
investments_count_investors$Percentage <- as.numeric(format(investments_count_investors$Count_by_Investments/
                                                                  sum(investments_count_investors$Count_by_Investments), 
                                                          scientific = FALSE, digits = 2))

        ## Plot No. of Investors by Number of Investments
ylim_investors_investments <- c(0, 1.1*max(investments_count_investors$Count_by_Investments))
xx_investors_investments <- barplot(investments_count_investors$Count_by_Investments, xaxt = 'n', xlab = '', width = 0.85,
                                  ylim = ylim_investors_investments, main = "Number of Investors by Number of Investments", 
                                  ylab = "Frequency")
text(x = xx_investors_investments, y = investments_count_investors$Count_by_Investments, 
     label = investments_count_investors$Count_by_Investments, pos = 3, cex = 0.8, col = "black")
axis(1, xx_investors_investments, labels=investments_count_investors$num_investments_cat, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 6.0 Number of Investors by Primary Organizations

        ## Summarize No. of Investors by Primary Organizations
prim_orgs_count_investors <- investors_WNA %>%
        dplyr::group_by(`Primary Organization`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Prim_Orgs = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Arrange DF
prim_orgs_count_investors <- arrange(prim_orgs_count_investors,
                                     desc(prim_orgs_count_investors$Count_by_Prim_Orgs))

        ## Add Percentage
prim_orgs_count_investors$Percentage <- as.numeric(format(prim_orgs_count_investors$Count_by_Prim_Orgs/
                                                       sum(prim_orgs_count_investors$Count_by_Prim_Orgs), scientific = FALSE, digits = 2))

        ## Take Top 20
prim_orgs_count_investors <- prim_orgs_count_investors[1:20,]
sum(prim_orgs_count_investors$Percentage)

        ## Plot No. of Investors by Primary Organizations
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

                ### Generate the layout. This function return a dataframe with one line per bubble. 
                ### It gives its center (x and y) and its radius, proportional of the value
prim_orgs_packing_investors <- circleProgressiveLayout(prim_orgs_count_investors$Count_by_Prim_Orgs, 
                                   sizetype='area')

                ### We can add these packing information to the initial data frame
data_prim_orgs_circlepack_investors <- cbind(prim_orgs_count_investors, prim_orgs_packing_investors)

                ### The next step is to go from one center + a radius to the coordinates of a circle that
                ### is drawn by a multitude of straight lines.
dat.gg_prim_orgs_circle_investors <- circleLayoutVertices(prim_orgs_packing_investors, npoints=50)

        ## Make the plot
HQ_Prim_Orgs_Investors_Circleplot <- ggplot() + 
        
                ### Make the bubbles
        geom_polygon(data = dat.gg_prim_orgs_circle_investors, aes(x, y, group = id, 
                                                       fill=as.factor(id)), 
                     colour = "black", alpha = 0.6) +
        
                ### Add text in the center of each bubble + control its size
        geom_text(data = data_prim_orgs_circlepack_investors, aes(x, y, size=Count_by_Prim_Orgs, 
                                                      label = `Primary Organization`)) +
        ggtitle("Distribution of Investors by Primary Organizations") +
        
                ### General theme:
        scale_size_continuous(range = c(1,4)) +
        theme_void() + 
        theme(legend.position="none") +
        coord_equal()

HQ_Prim_Orgs_Investors_Circleplot

        ## Write the Top 20 Table
write.table(prim_orgs_count_investors, file = "/Users/araf03/Desktop/MS Thesis/Tables/Investors/Top 20 Investors by Primary Organization Associated with.txt",
            sep = ",")






# 7.0 No of Investors by Operating Status

        ## Summarize No. of Investors by Operating Status
op_status_count_investors <- investors_WNA %>%
        dplyr::group_by(`Operating Status`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Op_Status = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Percentage of Investors by Operating Status
op_status_count_investors$Percentage <- as.numeric(format(op_status_count_investors$Count_by_Op_Status/
                                                           sum(op_status_count_investors$Count_by_Op_Status), 
                                                   scientific = FALSE, digits = 2))
        ## Write the Table
write.table(op_status_count_investors, file = "/Users/araf03/Desktop/MS Thesis/Tables/No. of Investors by Operating Status.txt",
            sep = ",")






# 8.0 No. of Investors by Investor Type

        ## Summarize No. of Investors by Investor Type
investor_type_count_investors <- investors_WNA %>%
        dplyr::group_by(`Investor Type`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Investor_Type = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Arrange DF
investor_type_count_investors <- arrange(investor_type_count_investors,
                                         desc(investor_type_count_investors$Count_by_Investor_Type))

        ## Percentage of Investors by Investor Type
investor_type_count_investors$Percentage <- as.numeric(format(investor_type_count_investors$Count_by_Investor_Type/
                                                                      sum(investor_type_count_investors$Count_by_Investor_Type), 
                                                              scientific = FALSE, digits = 2))

        ## Take Top 11
investor_type_count_investors <- investor_type_count_investors[1:11,]
sum(investor_type_count_investors$Percentage)

        ## Plot No. of Investors by Investor Type
ylim_investors_type <- c(0, 1.1*max(investor_type_count_investors$Count_by_Investor_Type))
xx_investors_type <- barplot(investor_type_count_investors$Count_by_Investor_Type, xaxt = 'n', xlab = '', width = 0.85,
                                    ylim = ylim_investors_type, main = "Number of Investors by Investor Types", 
                                    ylab = "Frequency")
text(x = xx_investors_type, y = investor_type_count_investors$Count_by_Investor_Type, 
     label = investor_type_count_investors$Count_by_Investor_Type, pos = 3, cex = 0.8, col = "black")
axis(1, xx_investors_type, labels=investor_type_count_investors$`Investor Type`, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)





