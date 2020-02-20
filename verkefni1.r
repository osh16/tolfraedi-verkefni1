tolur <- c(1,1,3,4,4,5,5,5,6,7,9)

# fjordungarmork (drasl skil ekki)
quantile(tolur)

# stadalfravik
sd(tolur)
mean(tolur)
median(tolur)

# ===========================
#"kaupverd";"teg_eign";"svfn";"byggar";"efstah";"haednr";"fjibmhl";"fjmib";"lyfta";"birtm2";"fjhaed";"fjbkar";"fjsturt";"fjklos";"fjeld";"fjherb";"fjstof";"fjgeym";"matssvaedi";"undirmatssvaedi"

library(tidyverse)

# saekjum gognin
ov <- read.table("husnaedisverd_2017.csv", header=TRUE, sep=";");
colnames(ov)

# breytum kaupverdi i alvoru verd
kaupverd_kr = (ov$kaupverd)*1000

# hversu margir fermetrar hver ibud hefur
fermetrar = ov$birtm2

# verd per fermeter
fermetra_verd = kaupverd_kr/fermetrar

# skiptum ur fjorum flokkum i tvo
teg_eign_groft <- forcats::fct_recode(ov$teg_eign, Sérbýli="Einbýlishús", Sérbýli="Parhús", Sérbýli="Raðhús", Íbúð="Íbúðareign")

# kodar fyrir kopav, seltjarnarn, og gardab
sveitarfelag <- c(1000,1100,1300)
eignir <- dplyr::filter(ov, svfn %in% sveitarfelag)

# breytum kodum i nofnin a baejarfelogum
eignir$svfn[eignir$svfn == 1000] = "Kopavogur"
eignir$svfn[eignir$svfn == 1100] = "Seltjarnarnes"
eignir$svfn[eignir$svfn == 1300] = "Gardabaer"

# breytum ur talnabreytum i flokkabreytur
eignir$svfn = factor(eignir$svfn, levels=c("Kopavogur","Seltjarnarnes","Gardabaer"))

ggplot(data=eignir, aes(svfn))+geom_bar()


