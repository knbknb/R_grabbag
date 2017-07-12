# knb 20170712
# "/home/knb/code/svn/eclipse38_dynlang/R_one-offs/R_grabbag"
# dc-vm11--storage--occupied-2017.R


# processing output of shell commands


# df -h output, grepped for "G"

data <- read.table(header = FALSE, text="16G        ./bbcp
1.5G       ./chelungpu
3.8G       ./chicxulub
14G        ./co2sink
40G        ./cosc
9.2G       ./deadsea
5.4G       ./dfdp
19G        ./donghai
13G        ./elgygytgyn
23G        ./far-deep
2.4G       ./hawaii
3.6G       ./iceland
6.2G       ./junin
39G        ./ohrid
1.1G       ./oman
14G        ./potrok
9.9G       ./sanandreas
65G        ./snakeriver
5.4G       ./van", sep="\t", stringsAsFactors = FALSE)

data$g <- as.numeric(gsub("(\\d+\\.?\\d).+$", "\\1", data$V1, perl=TRUE))
sum(data$g)


data2 <- read.table(header=FALSE, text="2.8M       ./andrill
26M        ./barberton
334M       ./bosumtwi
2.5M       ./cfddp
69M        ./chalco
4.8M       ./challa
89M        ./chesapeake
226M       ./coref
30M        ./corinth
2.0M       ./gonaf
15M        ./icdp
518M       ./icdp-sciconf-700
3.6M       ./kola
56M        ./koolau
186M       ./koyna
589M       ./ktb
106M       ./ktbhydraulic
150M       ./ktbto
442M       ./longvalley
38M        ./magadi
126M       ./mallik
146M       ./peten-itza
33M        ./qinghai
3.1M       ./reunion
2.7M       ./_selenium
229M       ./songliao
9.4M       ./titicaca
79M        ./tmp
300M       ./towuti
140M       ./unzen", sep="\t", stringsAsFactors = FALSE)

data2$MB <- as.numeric(gsub("(\\d+\\.?\\d).+$", "\\1", data2$V1, perl=TRUE))
sum(data2$MB)/1000
