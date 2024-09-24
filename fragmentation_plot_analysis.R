
# Landscape fragmentation analysis

# core forest
# plot definition with data labels
FC <- ggplot(data = core_patch, aes(x = Year, y = patch, 
                                    group = landscape_name, label=core))

# simple plot
FC +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "patch forest (km2)") +
  facet_wrap(~ landscape_name) + geom_line(color="black", size=0.8)+
  geom_point()


# Adding data label

FC +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "patch forest (km2)") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = core),
    nudge_x = 0,
    nudge_y = 800,
    check_overlap = TRUE,
    size = 3)

FC +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage forest cover") +
  facet_wrap(~ landscape_name) +
  geom_point()+
  geom_label(
    aes(label = forest_cover),
    nudge_x = 2,
    nudge_y = 0,
    check_overlap = TRUE,
    size = 2)


# Lets plot for human modified landscapes
HM <- ggplot(data = forest_HM2, aes(x = year, y = HM, 
                                    group = landscape_name, label=HM))
HM +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage human modified areas") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = HM),
    nudge_x = 1,
    nudge_y = 2,
    check_overlap = TRUE,
    size = 3)



# Now, lets create bar plots for change detection for HM

HM <- ggplot(data = forest_HM2_change, aes(x = Year, y = HM, 
                                           group = landscape_name, label=HM))
HM +
  geom_bar(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage change in human modified areas") +
  facet_wrap(~ landscape_name) + 
  geom_bar(stat="identity") + geom_bar(color="grey", size=1.2) +
  geom_text(
    aes(label = HM),
    nudge_x = 1,
    nudge_y = 2,
    check_overlap = TRUE,
    size = 3)

# Basic barplot
p<-ggplot(data=forest_HM2_change, aes(x=Year, y=HM, group = landscape_name, label=HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=HM), vjust=0, color="black", size=3.5)
p

p<-ggplot(data=forest_HM2_change, aes(x=Year, y=HM, group = landscape_name, label=HM)) +
  geom_bar(stat="identity", width=0.3)+ 
  labs(x = "year", y = "percentage change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light()+
  geom_text(aes(label=HM), vjust=-0.5, color="black", size=3.5)
p


# Now, lets create bar plots for change detection for forest cover
p<-ggplot(data=forest_HM2_change, aes(x=Year, y=forest_cover, group = landscape_name, label=HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=forest_cover), vjust=1, color="black", size=3.5)
p


# Now, lets create bar plots for annual average change in forest cover
p<-ggplot(data=forest_HM2_change, aes(x=Year, y=rate_fc, group = landscape_name, label=HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_fc), vjust=0.9, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in HM
p<-ggplot(data=forest_HM2_change, aes(x=Year, y=rate_HM, group = landscape_name, label=HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_HM), vjust=-0.2, color="black", size=3.5)
p





# --------------------------------------------------------------------------------

# Protected Area Analysis

# lets select data for NP first
NP<-forest_HM2_PA_change1[c(1:3,10:15,22:27,34:36,40:42, 52:54),]
print(NP)

# lets create bar plots for change detection for forest cover
p<-ggplot(data=NP, aes(x=Year, y=forest_cover, group = landscape_name, label=forest_cover)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=forest_cover), vjust=0.8, color="black", size=3.5)
p

# lets create bar plots for change detection for HM
p<-ggplot(data=NP, aes(x=Year, y=HM, group = landscape_name, label=HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=HM), vjust=-0.2, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in HM
p<-ggplot(data=NP, aes(x=Year, y=rate_HM, group = landscape_name, label=rate_HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_HM), vjust=-0.2, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in forest cover
p<-ggplot(data=NP, aes(x=Year, y=rate_fc, group = landscape_name, label=rate_fc)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_fc), vjust=0.9, color="black", size=3.5)
p




# lets create trend plots for forest cover within NP

# lets start by selecting data for NP first
NP<-forest_HM2_PA1[c(1:4,13:20,29:36,45:48,53:56, 69:72),]
print(NP)

FC <- ggplot(data = NP, aes(x = Year, y = forest_cover, 
                            group = landscape_name, label=forest_cover))

FC +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage forest cover") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = forest_cover),
    nudge_x = 2,
    nudge_y = 6,
    check_overlap = TRUE,
    size = 3)

# lets create trend plots for HM within NP

HM <- ggplot(data = NP, aes(x = Year, y = HM, 
                            group = landscape_name, label=HM))
HM +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage human modified areas") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = HM),
    nudge_x = 0,
    nudge_y = 0.8,
    check_overlap = TRUE,
    size = 3)






# --------------------------------------------------------------------------------

# Forest reserves Analysis

# lets select data for forest reserves
FR<-forest_HM2_PA_change1[c(4:6,16:18,28:30,37:39,43:45, 49:51, 55:60),]
print(FR)

# lets create bar plots for change detection for forest cover
p<-ggplot(data=FR, aes(x=Year, y=forest_cover, group = landscape_name, label=forest_cover)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=forest_cover), vjust=1, color="black", size=3.5)
p

# lets create bar plots for change detection for HM
p<-ggplot(data=FR, aes(x=Year, y=HM, group = landscape_name, label=HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=HM), vjust=-0.2, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in HM
p<-ggplot(data=FR, aes(x=Year, y=rate_HM, group = landscape_name, label=rate_HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_HM), vjust=0.5, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in forest cover
p<-ggplot(data=FR, aes(x=Year, y=rate_fc, group = landscape_name, label=rate_fc)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_fc), vjust=1, color="black", size=3.5)
p




# lets create trend plots for forest cover within FR

# lets start by selecting data for FR data
FR<-forest_HM2_PA1[c(5:8,21:24,37:40,49:52,57:60,65:68,73:80),]
print(FR)

FC <- ggplot(data = FR, aes(x = Year, y = forest_cover, 
                            group = landscape_name, label=forest_cover))

FC +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage forest cover") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = forest_cover),
    nudge_x = 1,
    nudge_y = 8,
    check_overlap = TRUE,
    size = 3)

# lets create trend plots for HM within FR

HM <- ggplot(data = FR, aes(x = Year, y = HM, 
                            group = landscape_name, label=HM))
HM +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage human modified areas") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = HM),
    nudge_x = 0,
    nudge_y = 0.8,
    check_overlap = TRUE,
    size = 3)





# --------------------------------------------------------------------------------


# lets select data for community hunting zones and wildlife sanctuaries
CHZ<-forest_HM2_PA_change1[c(7:9,19:21,31:33,46:48),]
print(CHZ)

# lets create bar plots for change detection for forest cover
p<-ggplot(data=CHZ, aes(x=Year, y=forest_cover, group = landscape_name, label=forest_cover)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=forest_cover), vjust=1, color="black", size=3.5)
p

# lets create bar plots for change detection for HM
p<-ggplot(data=CHZ, aes(x=Year, y=HM, group = landscape_name, label=HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=HM), vjust=-0.1, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in HM
p<-ggplot(data=CHZ, aes(x=Year, y=rate_HM, group = landscape_name, label=rate_HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_HM), vjust=0.5, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in forest cover
p<-ggplot(data=CHZ, aes(x=Year, y=rate_fc, group = landscape_name, label=rate_fc)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_fc), vjust=1, color="black", size=3.5)
p




# lets create trend plots for forest cover within community hunting zones and SC

# lets start by selecting data for FR data
SC<-forest_HM2_PA1[c(9:12,25:28,41:44,61:64),]
print(SC)

FC <- ggplot(data = SC, aes(x = Year, y = forest_cover, 
                            group = landscape_name, label=forest_cover))

FC +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage forest cover") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = forest_cover),
    nudge_x = 1,
    nudge_y = 2,
    check_overlap = TRUE,
    size = 3)

# lets create trend plots for HM within FR

HM <- ggplot(data = SC, aes(x = Year, y = HM, 
                            group = landscape_name, label=HM))
HM +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage human modified areas") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = HM),
    nudge_x = 0,
    nudge_y = 0.2,
    check_overlap = TRUE,
    size = 3)




# --------------------------------------------------------------------------------


# lets select data for community hunting zones and wildlife sanctuaries
CHZ<-forest_HM2_PA_change1[c(7:9,19:21,31:33,46:48),]
print(CHZ)

# lets create bar plots for change detection for forest cover
p<-ggplot(data=CHZ, aes(x=Year, y=forest_cover, group = landscape_name, label=forest_cover)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=forest_cover), vjust=1, color="black", size=3.5)
p

# lets create bar plots for change detection for HM
p<-ggplot(data=CHZ, aes(x=Year, y=HM, group = landscape_name, label=HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=HM), vjust=-0.1, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in HM
p<-ggplot(data=CHZ, aes(x=Year, y=rate_HM, group = landscape_name, label=rate_HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_HM), vjust=0.5, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in forest cover
p<-ggplot(data=CHZ, aes(x=Year, y=rate_fc, group = landscape_name, label=rate_fc)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_fc), vjust=1, color="black", size=3.5)
p




# lets create trend plots for forest cover within community hunting zones and SC

# lets start by selecting data for FR data
SC<-forest_HM2_PA1[c(9:12,25:28,41:44,61:64),]
print(SC)

FC <- ggplot(data = SC, aes(x = Year, y = forest_cover, 
                            group = landscape_name, label=forest_cover))

FC +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage forest cover") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = forest_cover),
    nudge_x = 1,
    nudge_y = 2,
    check_overlap = TRUE,
    size = 3)

# lets create trend plots for HM within FR

HM <- ggplot(data = SC, aes(x = Year, y = HM, 
                            group = landscape_name, label=HM))
HM +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage human modified areas") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = HM),
    nudge_x = 0,
    nudge_y = 0.2,
    check_overlap = TRUE,
    size = 3)







# --------------------------------------------------------------------------------


# Analysis for all Pa combined


# lets create bar plots for change detection for forest cover
p<-ggplot(data=all_PA_change, aes(x=Year, y=forest_cover, group = landscape_name, label=forest_cover)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=forest_cover), vjust=1, color="black", size=3.5)
p

# lets create bar plots for change detection for HM
p<-ggplot(data=all_PA_change, aes(x=Year, y=HM, group = landscape_name, label=HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "percentage change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=HM), vjust=-0.1, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in HM
p<-ggplot(data=all_PA_change, aes(x=Year, y=rate_HM, group = landscape_name, label=rate_HM)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in human modified areas") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_HM), vjust=0.1, color="black", size=3.5)
p

# Now, lets create bar plots for annual average change in forest cover
p<-ggplot(data=all_PA_change, aes(x=Year, y=rate_fc, group = landscape_name, label=rate_fc)) +
  geom_bar(stat="identity", width=0.3, fill="dark grey")+ 
  labs(x = "change periods", y = "annual rate of change in forest cover") +
  facet_wrap(~ landscape_name)+
  theme_light() + geom_text(aes(label=rate_fc), vjust=1, color="black", size=3.5)
p




# lets create trend plots for forest cover 


FC <- ggplot(data = all_PA, aes(x = Year, y = forest_cover, 
                                group = landscape_name, label=forest_cover))

FC +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage forest cover") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = forest_cover),
    nudge_x = 1,
    nudge_y = 2,
    check_overlap = TRUE,
    size = 3)

# lets create trend plots for HM 

HM <- ggplot(data = all_PA, aes(x = Year, y = HM, 
                                group = landscape_name, label=HM))
HM +
  geom_line(show.legend = FALSE) +
  theme_light() + 
  labs(x = "year", y = "percentage human modified areas") +
  facet_wrap(~ landscape_name) + 
  geom_point() + geom_line(color="grey", size=1.2)+geom_point(color="grey", size=2)+
  geom_text(
    aes(label = HM),
    nudge_x = 0,
    nudge_y = 0.2,
    check_overlap = TRUE,
    size = 3)











