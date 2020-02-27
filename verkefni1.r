tolur <- c(1,1,3,4,4,5,5,5,6,7,9)

# fjordungarmork (drasl skil ekki)
quantile(tolur)

# stadalfravik
sd(tolur)
mean(tolur)
median(tolur)

# ===========================
#"kaupverd";"teg_eign";"svfn";"byggar";"efstah";"haednr";"fjibmhl";"fjmib";"lyfta";"birtm2";"fjhaed";"fjbkar";"fjsturt";"fjklos";"fjeld";"fjherb";"fjstof";"fjgeym";"matssvaedi";"undirmatssvaedi"

library(ggplot2)
library(methods)
options(scipen=5)

# saekjum gognin
ov <- read.table("husnaedisverd_2017.csv", header=TRUE, sep=";");
colnames(ov)

# breytum kaupverdi i alvoru verd
ov$kaupverd = ov$kaupverd*1000

# hversu margir fermetrar hver ibud hefur
ov$fermetraverd = ov$kaupverd/ov$birtm2

# skiptum ur fjorum flokkum i tvo
ov$teg_eign_groft <- forcats::fct_recode(ov$teg_eign, Sérbýli="Einbýlishús", Sérbýli="Parhús", Sérbýli="Raðhús", Íbúð="Íbúðareign")

# kodar fyrir kopav, seltjarnarn, og gardab
sveitarfelag <- c(1000,1100,1300)
eignir <- dplyr::filter(ov, svfn %in% sveitarfelag)

# breytum kodum i nofnin a baejarfelogum
eignir$svfn[eignir$svfn == 1000] = "Kopavogur"
eignir$svfn[eignir$svfn == 1100] = "Seltjarnarnes"
eignir$svfn[eignir$svfn == 1300] = "Gardabaer"

# breytum ur talnabreytum i flokkabreytur
eignir$svfn = factor(eignir$svfn, levels=c("Kopavogur","Seltjarnarnes","Gardabaer"))

# mynd sem synir fjolda eigna i sveitarfelogunum 
p <- ggplot(data=eignir, aes(eignir$svfn, fill=teg_eign_groft)) + geom_bar()
show(p)

# drasl
# mynd sem synir fermetraverd eigna
p <- ggplot(data=eignir, aes(x=eignir$fermetraverd, fill=eignir$svfn)) + geom_histogram()# + xlim(c(min(eignir$fermetraverd),600000))
show(p)

# mynd sem synir byggingarar eigna eftir sveitarfelogum
p <- ggplot(data=eignir, aes(x=eignir$byggar, fill=eignir$svfn)) + geom_bar()
show(p)

# Teiknið mynd sem sýnir stærð sérbýla eftir sveitafélögum
serbyli = dplyr::filter(eignir, grepl("Sérbýli", eignir$teg_eign_groft))
p <- ggplot(data=serbyli, aes(x=serbyli$birtm2, fill=serbyli$svfn)) + geom_histogram()
show(p)

# Teiknið mynd sem sýnir fermetraverð íbúða eftir sveitafélögum
p <- ggplot(data=eignir, aes(x=eignir$fermetraverd, fill=eignir$svfn)) + geom_bar()
show(p)

# p)
medal_fverd_eignir <- mean(eignir$fermetraverd)
stadal_fverd_eignir <- sd(eignir$fermetraverd)
Meðaltal fermetraverðs á eignunum er `r mean(eignir$fermetraverd)` og staðalfrávikið er `r sd(eignir$fermetraverd)`

# q)
kop = dplyr::filter(eignir, grepl("Kopavogur", eignir$svfn))
gar = dplyr::filter(eignir, grepl("Gardabaer", eignir$svfn))
nes = dplyr::filter(eignir, grepl("Seltjarnarnes", eignir$svfn))
mean(kop$byggar, na.rm=TRUE)
mean(gar$byggar, na.rm=TRUE)
mean(nes$byggar, na.rm=TRUE)

# r)
badkor <- dplyr::filter(eignir,teg_eign_groft=="Sérbýli",fjbkar==2)
nrow(badkor)

# s)
prop.table(table(eignir$teg_eign_groft, eignir$svfn))
table(eignir$teg_eign_groft, eignir$svfn)

# t)
prop.table(table(eignir$teg_eign_groft, eignir$svfn))

# u)
eignir %>%
  group_by(svfn,teg_eign_groft) %>%
  summarise(meðaltal = mean(fermetraverd), miðgildi = median(fermetraverd), staðalfrávik = sd(fermetraverd),fjoldi=n())

# v)
eignir %>%
  group_by(svfn,teg_eign_groft="Sérbýli") %>%
  summarise(meðaltal = mean(birtm2), miðgildi = median(birtm2), staðalfrávik = sd(birtm2))
