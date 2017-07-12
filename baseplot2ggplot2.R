library(ggplot2)

df <- read.table(stringsAsFactors = FALSE, header = TRUE, text = "sample  element ratio
1   0.6175  0.063568046
2   4.678133548 0.008924568
3   3.638120051 0.005707297
4   4.116887372 0.007867378
5   3.090387742 0.059814081
6   10.3098635  0.040600128
7   10.29649952 0.019962218
8   8.241753356 0.012910088
9   26.76850701 0.294731393
10  8.136495793 0.031161747
11  7.993317894 0.021593337
12  10.48361696 0.025802074
13  14.37169851 0.030482194
14  13.19394369 0.032786504
15  3.756483892 0.027182974
16  12.21848391 0.032594756
17  6.560291802 0.049019108
18  10.71912771 0.072430938
19  1.708155726 0.007771124
20  10.0684893  0.054408104
21  16.38043254 0.076730258
22  11.50283707 0.044073631
23  10.11554913 0.064254448
24  13.52364219 0.0707148
25  10.51594695 0.060657727
26  14.51045122 0.07431958
27  16.46248665 0.059498267
28  12.17582522 0.04927026
29  20.83513461 0.059772704
30  18.10278099 0.069570053
31  8.404223175 0.051083018
32  13.96869871 0.069607707
33  9.396932425 0.044899193
34  12.39775022 0.066800258
35  1.625963393 0.009627417
36  16.9934813  0.059799741
37  75.5    0.3775
38  73  0.350961538
39  45.3    0.191949153
40  92.5    0.409292035
41  39.8    0.176106195
42  48.2    0.194354839
43  48.9    0.188076923
44  52.6    0.219166667
45  44.4    0.1734375
46  53.9    0.245
47  168.4   0.559468439
48  165.8   0.587943262
49  87.9    0.412676056
50  99.5    0.328382838
51  105.9   0.318975904
52  172.7   0.579530201
53  190.4   0.602531646
54  206.6   0.586931818
55  101 0.321656051
56  101.8   0.329449838
57  117.7   0.346176471
58  97.9    0.337586207
59  90.5    0.285488959
60  182.1   0.550151057
61  109.9   0.399636364
62  8   0.053619303
63  4.9 0.039294306
64  4.7 0.035768645
65  6.3 0.041888298
66  8.3 0.035698925
67  6.7 0.043733681
68  25.2    0.193548387
69  24.7    0.168600683
70  59.1    0.226610429
71  30.6    0.234303216
72  45.1    0.223710317
73  93.2    0.315611243
74  83.1    0.309151786
75  88.8    0.311360449
76  34.8    0.169014085
77  36.6    0.183550652
79  4.47    0.071565802
80  1.62    0.012413793
81  2.08    0.021010101
82  1.27    0.012330097
83  1.34    0.019705882
84  2.54    0.030274136
85  3.96    0.057641921
86  32.9    0.115438596
87  19.4    0.067595819
88  27.6    0.088461538
89  14.5    0.050699301
90  31  0.096875
91  36.3    0.078232759
92  27.8    0.072395833
93  8.56    0.039447005
94  10.1    0.040239044
95  4.44    0.056060606
96  3.94    0.05317139
97  4.18    0.059123055
98  5.23    0.07568741")

el_x1 <- df$element
el_y1 <- df$ratio
par(fin = c (5,5), mar = c(5,5,6,1), xpd = "true")
plot (xlim = c(0.1,250), ylim = c(0.003,4), el_x1, el_y1, log = "xy", xlab = "element (ppm)", ylab = "ratio", pch = 0, cex = 0.7, col = "orange1", las = 1, cex.axis = 1, cex.lab = 1.8, yaxs = "i", xaxs = "i")
# points (el_x2, el_y2, pch = 3, cex = 0.7, col = "olivedrab2", las = 1, yaxs = "i", xaxs = "i")
# points (el_x3, el_y3, pch = 4, cex = 0.7, col = "darkseagreen1", las = 1, yaxs = "i", xaxs = "i")
# points (el_x4, el_y4, pch = 2, cex = 0.7, col = "khaki1", las = 1, yaxs = "i", xaxs = "i")
# points (el_x5, el_y5, pch = 1, cex = 0.7, col = "lavender", las = 1, yaxs = "i", xaxs = "i")
# points (el_x6, el_y6, pch = 15, cex = 0.7, col = "seagreen", las = 1, yaxs = "i", xaxs = "i")
# points (el_x7, el_y7, pch = 15, cex = 1, col = "indianred1", las = 1, yaxs = "i", xaxs = "i")
# points (el_x8, el_y8, pch = 16, cex = 1, col = "royalblue1", las = 1, yaxs = "i", xaxs = "i")
# points (el_x9, el_y9, pch = 15, cex = 1, col = "red2", las = 1, yaxs = "i", xaxs = "i")
# points (el_x10, el_y10, pch = 16, cex = 1, col = "navy", las = 1, yaxs = "i", xaxs = "i")
# points (el_x11, el_y11, pch = 16, cex = 1, col = "darkmagenta", las = 1, yaxs = "i", xaxs = "i")
legend ("topright", inset=c(-0.46,0), legend=c("df1verylongname", "df2verylongname", "df3verylongname", "df4verylongname", "df5verylongname", "df6verylongname", "df7verylongname","df8verylongname", "df9verylongname", "df10verylongname", "df11verylongname"), pch=c(0,3,4,2,1,15,15,15,16,16,16), col=c("orange1","olivedrab2","darkseagreen1","khaki1", "lavender", "seagreen", "indianred1", "red2", "royalblue2", "navy", "darkmagenta"))

# Finish the function draw_pop_legend
ggplot(df, aes(element, ratio, fill="Element 1")) +
        geom_point(color="red2") +
        #geom_point(color=factor(1), size=1)
        coord_cartesian( xlim=c(0, 300), ylim=c(0.0003, 4)) +
        scale_x_continuous(labels=c(0, "hundred", 200, ">=300"), breaks=c(0, 100, 200, 300)) +
        labs(x = "X axis", y = "Y axis (ratio/ppm)", fill = "Elements") +
        theme_minimal() +
        theme(#legend.position = "right",
              legend.position = c(0.05, 0.95),
              legend.justification = c(0.05, 0.95),
              axis.title.x = element_text(color="black", hjust = 0),
              axis.title.y = element_text(color="black", hjust = 0))

#draw_pop_legend()
