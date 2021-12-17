# Maxwell J Farrell
# maxwell.farrell@utoronto.ca

require(dplyr)
require(tidyr)
require(ggplot2)
require(cowplot)

medic <- read.table("./WOS_medic_sep102021.txt", sep="\t", header=T)
medic_NLP <- read.table("./WOS_medic_NLP_sep102021.txt", sep="\t", header=T)

eco <- read.table("./WOS_ecoevo_sep102021.txt", sep="\t", header=T)
eco_NLP <- read.table("./WOS_ecoevo_NLP_sep102021.txt", sep="\t", header=T)

names(medic)[1] <- "year"
names(medic_NLP)[1] <- "year"
names(eco)[1] <- "year"
names(eco_NLP)[1] <- "year"

names(medic)[3] <- "percent_total"
names(medic_NLP)[3] <- "percent_total"
names(eco)[3] <- "percent_total"
names(eco_NLP)[3] <- "percent_total"

medic$Subject <- "biomedical"
medic_NLP$Subject <- "biomedical"
eco$Subject <- "ecology / evolution"
eco_NLP$Subject <- "ecology / evolution"

medic$NLP <- "Total papers per discipline"
medic_NLP$NLP <- "Text mining papers"
eco$NLP <- "Total papers per discipline"
eco_NLP$NLP <- "Text mining papers"

dat <- rbind(medic, medic_NLP, eco, eco_NLP)

names(dat)[2] <- "records"

dat$Subject <- factor(dat$Subject, levels = c("biomedical", "ecology / evolution"))

# subset to exclude 2021
dat <- dat[dat$year<=2020,]

# subset to 1990 and after
dat <- dat[dat$year>=1990,]


colors <- c("#C0C0C0","#009e73")

dat_tm <- dat[dat$NLP=="Text mining papers",]

p_tot <- ggplot(dat_tm, aes(fill=Subject, y=records, x=year)) + 
    geom_bar(position="stack", stat="identity") + 
    scale_fill_manual(values=colors) + 
    theme_classic() +
    scale_y_continuous(expand = c(0, 0), labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + 
        xlim(1990,2020) +
    scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
    xlab("Year") + ylab("Number of text mining papers") +
    theme(legend.position = c(0.22, 0.9), 
            strip.background = element_blank(),
            strip.text.x = element_text(size=10, face="bold", hjust=0.02),
            legend.title = element_blank())  
p_tot


# filling in zeros for years with no records
dat_prop <- dat %>% complete(year, nesting(Subject, NLP), fill = list(records = 0))
# remove percent-total
dat_prop <- select(dat_prop, -percent_total)
dat_prop <- spread(dat_prop, key = "NLP", value="records")
names(dat_prop)[3:4] <- c("TM","TOTAL")
dat_prop <- dat_prop %>% group_by(Subject,year) %>% summarise(prop_TM=TM/TOTAL)


p_prop <- ggplot(dat_prop, aes(color=Subject, y=prop_TM, x=year)) + 
    geom_line(position="stack", stat="identity", colors=colors, size=1, aes(linetype=Subject)) + 
    scale_color_manual(values=colors) + theme_classic() +
    scale_y_continuous(expand = c(0, 0)) + xlim(1990,2020) +
    scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
    xlab("Year") + ylab("Proportion of papers with text mining") +
    theme(legend.position = c(0.22, 0.9), legend.title = element_blank()) #+ theme(legend.position = "none")
p_prop


p_combo <- plot_grid(p_tot, p_prop, nrow=1,#rel_widths=c(1.2,1),
            # labels = c('A) Text mining papers', 'B) Total papers per discipline'))
            labels = c('A)', 'B)'), hjust = -2, align="v")

p_combo

ggsave("trends_combo_plot.pdf", p_combo, width=8.5, height=3.5)
ggsave("trends_combo_plot.png", p_combo, width=8.5, height=3.5)
