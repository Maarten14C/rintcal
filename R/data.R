#' @name intcal.data
#' @title plot the IntCal20 data
#' @description plot the C14 ages underpinning the IntCal20/Marine20/SHCal20 calibration curves
#' @details These datasets were downloaded from Intcal.org. All data have both uncertainties in C14 age and on the calendar scale. For trees this is the sample thickness (e.g., 10 years or 1 year).
#' The name of each dataset starts with a lower-case letter which indicates their nature (t = tree-rings, l = lake sediment, c = coral, m = marine sediment, s = speleothem), followed by either the radiocarbon laboratory's placename or the lastname of the main author. Most of the tree-ring datasets are dated at calendar year precision; tSeattle (references 1-2), tBelfast (3-5), tWaikato (4-7), tGroningen (8-10), tHeidelberg (11-14), tPretoria (16), tIrvine (17-20), tGalimberti (21), tMannheim (22-25), tAix (26-27), tAarhus (22, 28-30), tManningKromer (31-32), tVienna (33-34), tTokyo (35-39), tArizona (40), tMiyake (41), tPearson (22, 41-45), and tZurich (22-23, 25, 41, 43, 46-49). Horizontal error bars for these series indicate the numbers of rings in the samples (e.g., 10 tree-rings; 1-yr samples do not have error bars).
#' Additionally, there are some floating tree-ring datasets with imprecisely known calendar ages; tAdolphy (50) and tTurney (51-52). For these and the following datasets, horizontal error bars indicate their 1 sd calendar age uncertainties.
#' Beside trees, other datasets include lake sediment (lSuigestu, 53-54), corals (cBard 55-56, cFairbanks 57, cCutler 58 and cDurand 61, marine sediment (mCariaco 59-60, 62-63, mBard 64-65) and speleothems (sSouthon 66-67, sHoffman 68, sBeck 69).
#' The southern hemisphere calibration curve SHCal20 is mostly modelled on IntCal20, but it contains datasets from the southern hemisphere; tPretoria (70), tWaikato (72-75), tBelfast (76-67), tSydney (78-80), tLivermore (81), tArizona, tIrvineWaikato and tZurich (82-83).
#' @return A plot of the IntCal curve and the underlying data, as well as (invisibly) the data themselves
#' @param cal1 First calendar year for the plot
#' @param cal2 Last calendar year for the plot
#' @param cc1 Name of the calibration curve. Can be "IntCal20", "Marine20", "SHCal20", or for the previous curves "IntCal13", "Marine13" or "SHCal13".
#' @param cc2 Optional second calibration curve to plot. Can be "IntCal20", "Marine20", "SHCal20", or for the previous curves "IntCal13", "Marine13" or "SHCal13". Defaults to nothing, NA.
#' @param calcurve.data Which dataset to use. Defaults to \code{calcurve.data="IntCal20"}, but can also be \code{calcurve.data="SHCal20"}. Note that Marine20 is based on IntCal20 and a marine carbon cycle model.
#' @param realm Which 'realm' of radiocarbon to use. Defaults to \code{realm="C14"} but can also be set to \code{realm="F14C"}, \code{realm="pMC"} or \code{realm="D14C"}. Can be shorted to, respectively, "C", "F", "P" or "D" (or their lower-case equivalents).
#' @param select.sets Which datasets to plot. Defaults to all datasets within the selected period.
#' @param BCAD The calendar scale of graphs and age output-files is in cal BP (calendar or calibrated years before the present, where the present is AD 1950) by default, but can be changed to BC/AD using \code{BCAD=TRUE}.
#' @param cal.lab The labels for the calendar axis (default \code{age.lab="cal BP"} or \code{"BC/AD"} if \code{BCAD=TRUE}), or to \code{age.lab="kcal BP"} etc. if ka=TRUE.
#' @param cal.rev Reverse the calendar axis.
#' @param c14.lab Label for the C-14 axis. Defaults to 14C BP (or 14C kBP if ka=TRUE).
#' @param c14.lim Axis limits for the C-14 axis. Calculated automatically by default.
#' @param c14.rev Reverse the C-14 axis.
#' @param ka Use kcal BP (and C14 kBP).
#' @param cc1.col Colour of the calibration curve (outline).
#' @param cc1.fill Colour of the calibration curve (fill).
#' @param cc2.col Colour of the calibration curve (outline), if activated (default cc2=NA).
#' @param cc2.fill Colour of the calibration curve (fill), if activated (default cc2=NA).
#' @param data.cols colours of the data points. Defaults to R's colours 1 to 8 (black, red, green, darkblue, lightblue, purple, orange, and grey)
#' @param data.pch Symbols of the data points. Defaults to R's symbols 1, 2, 5, 6, and 15 to 19 (open circle, open upward triangle, open diamond, open downward triangle, closed square, closed circle, closed upward triangle, closed diamond)
#' @param pch.cex Size of the data symbols. Defaults to 0.5.
#' @param legend.loc Location of the data legend. Defaults to topleft. Set to NA for no plotting.
#' @param legend.ncol Number of columns of the data legend.
#' @param legend.cex Size of the legend. Defaults to 0.7.
#' @param cc.legend Location of the legend for the calibration curve(s).
#' @param bty Box type around the plot. Defaults to "l"-shaped.
#' @param ... Any additional optional plotting parameters.
#' @examples
#'   intcal.data(100, 200)
#'   intcal.data(40e3, 55e3, ka=TRUE)
#'   # plot Suigetsu and Cariaco data only
#'   dat <- intcal.data(20e3, 25e3)
#'   unique(dat$set) # ordered against their appearance in the plot's legend
#'   dat <- intcal.data(20e3, 25e3, select.sets=c(109, 120), data.cols=c(1,2))
#' @references
#' [1]Stuiver, M, and Braziunas, TF. 1993. Sun, ocean, climate and atmospheric 14CO2: an evaluation of causal and spectral relationships. Holocene 3: 289-305.
#'
#' [2] Stuiver, M, Reimer, PJ, Braziunas, TF. 1998. High-precision radiocarbon age calibration for terrestrial and marine samples. Radiocarbon 40:1127-1151.
#'
#' [3] McCormac FG, Bayliss A, Baillie MGL, Brown DM. 2004. Radiocarbon calibration in the Anglo-Saxon period: AD 495-725. Radiocarbon 46(3):1123-5.
#'
#' [4] McCormac, FG, Hogg, AG, Higham, TFG, Lynch-Stieglitz, J, Broecker, WS, Baillie, MGL, Palmer, J, Xiong, L, Pilcher, JR, Brown, D, Hoper, ST, 1998. Temporal variation in the interhemispheric C-14 offset. Geophysical Research Letters 25, 1321-1324.
#'
#' [5] Pearson, G. W., Pilcher, J. R., Baillie, M. G. L., Corbett, D. M., and Qua, F. (1986). High-Precision C-14 Measurement of Irish Oaks to Show the Natural C-14 Variations from AD 1840 to 5210 BC Radiocarbon 28: 911-934.
#'
#' [6] Hogg, A. G., McCormac, F. G., Higham, T. F. G., Reimer, P. J., Baillie, M. G. L., and Palmer, J. G. (2002). High-precision radiocarbon measurements of contemporaneous tree-ring dated wood from the British Isles and New Zealand: AD 1850-950. Radiocarbon 44: 633-640.
#'
#' [7] Hogg, A., Palmer, J., Boswijk, G., Reimer, P., and Brown, D. (2009). Investigating the interhemispheric C-14 offset in the 1st millennium AD and assessment of laboratory bias and calibration errors. Radiocarbon 51, 1177-1186.
#'
#' [8] de Jong, A. F. M., Becker, B., and Mook, W. G. (1986). High-precision calibration of the radiocarbon time scale, 3930-3230 cal BC. Radiocarbon 28: 939-941.
#'
#' [9] de Jong, AFM, Becker, B, Mook, WG, 1989. Corrected calibration of the radiocarbon time scale. Radiocarbon 31:201-210.
#'
#' [10] Kuitems, M, Plicht, Jvd, Jansma, E, Wood from the Netherlands around the time of the Santorini eruption dated by dendrochronology and Radiocarbon. Radiocarbon this issue.
#'
#' [11] Kaiser, K. F., M. Friedrich, C. Miramont, B. Kromer, M. Sgier, M. Schaub, I. Boeren, S. Remmele, S. Talamo, F. Guibal and O. Sivan (2012). 'Challenging process to make the Lateglacial tree-ring chronologies from Europe absolute-an inventory.' Quaternary Science Reviews 36: 78-90.
#'
#' [12] Kromer, B., and Spurk, M. (1998). Revision and tentative extension of the tree-ring based C-14 calibration, 9200-11,855 cal BP. Radiocarbon 40: 1117-1125.
#'
#' [13] Kromer, B., Manning, S.W., Kuniholm, P.I., Newton, M.W., Spurk, M. & Levin, I. 2001. Regional 14CO2 offsets in the troposphere: magnitude, mechanisms, and consequences. Science 294: 2529-2532.
#'
#' [14] Kromer, B., S. W. Manning, M. Friedrich, S. Talamo and N. Trano (2010). 14C Calibration in the 2nd and 1st Millennia BC Eastern Mediterranean Radiocarbon Comparison Project (EMRCP). Radiocarbon 52(3): 875-886.
#'
#' [15] Hua, Q., Barbetti, M., Fink, D., Kaiser, K. F., Friedrich, M., Kromer, B., Levchenko, V. A., Zoppi, U., Smith, A. M., and Bertuch, F. (2009). Atmospheric 14C variations derived from tree rings during the early Younger Dryas. Quaternary Science Reviews 28, 2982-2990.
#'
#' [16] Vogel, J. C., and van der Plicht, J. (1993). Calibration Curve for Short-Lived Samples, 1900-3900 BC. Radiocarbon 35: 87-91.
#'
#' [17] Taylor, R.E. and Southon, J., (2013). Reviewing the Mid-First Millennium BC 14C 'warp' using 14C/bristlecone pine data. NIMB Research Section B: Beam Interactions with Materials and Atoms, 294, pp.440-443
#'
#' [18] Hogg, A., Southon, J., Turney, C., Palmer, J., Ramsey, C.B., Fenwick, P., Boswijk, G., Buentgen, U., Friedrich, M., and Helle, G., 2016. Decadally resolved lateglacial radiocarbon evidence from New Zealand kauri: Radiocarbon, v. 58, p. 709
#'
#' [19] Park, J., Southon, J., Fahrni, S., Creasman, P., & Mewaldt, R. (2017). Relationship between solar activity and D14C peaks in AD 775, AD 994, and 660 BC. Radiocarbon, 59(4), 1147-1156. doi:10.1017/RDC.2017.59
#'
#' [20] Simon M. Fahrni, John Southon, Benjamin T. Fuller, Junghun Park, Michael Friedrich, Raimund Muscheler, Lukas Wacker, R. E. Taylor;Single-year German oak and Californian bristlecone pine 14C data at the beginning of the Hallstatt plateau from 856 BC to 626 BC; Radiocarbon
#'
#' [21] Galimberti, M, Bronk Ramsey, C, and Manning, S W, 2004 Wiggle-match dating of tree-ring sequences, Radiocarbon, 46, 917-24
#'
#' [22] Friedrich, R, Kromer, B, Wacker, L, Olsen, J, Remmele, S, Lindauer, S, Land, A, Pearson, C. A new annual 14C dataset for calibrating the Thera eruption. Radiocarbon this issue
#'
#' [23] Sookdeo A, Kromer B, Buentgen U, Friedrich M, Friedrich R, Helle G, Pauly M, Nievergelt D, Reinig F, Treydte K, Synal HA, Wacker, L 2019a. Quality Dating: A well-defined protocol implemented at ETH for high-precision 14C dates tested on Late Glacial wood. Radiocarbon.
#'
#' [24] Friedrich, R, Kromer, B, Sirocko, F, Esper, J, Lindauer, S, Nievergelt, D, Heussner, K, Westphal, T.  Annual 14C tree-ring data around 400AD: mid and high-latitude records. Radiocarbon:   in press
#'
#' [25] Usoskin, I. G., B. Kromer, F. Ludlow, J. Beer, M. Friedrich, G. A. Kovaltsov, S. K. Solanki and L. Wacker (2013). 'The AD775 cosmic event revisited: the Sun is to blame.' Astronomy & Astrophysics 552(L3): 1-4.
#'
#' [26] Capano, M, Miramont, C, Guibal, F, Kromer, B, Tuna, T, Fagault, Y, Bard, E, 2018. Wood 14C Dating with AixMICADAS: Methods and Application to Tree-Ring Sequences from the Younger Dryas Event in the Southern French Alps. Radiocarbon 60, 51-74.
#'
#' [27]C apano M, Miramont C, Shindo, L, Guibal F, Marschal, C, Kromer B, Tuna T, Bard E. Onset of the Younger Dryas recorded with 14C at annual resolution in French subfossil trees. Radiocarbon, 2020.
#'
#' [28] Fogtmann-Schulz, A, Kudsk, SGK, Trant, PLK, Baittinger, C, Karoff, C, Olsen, J, Knudsen, MF, 2019. Variations in Solar Activity Across the Spoerer Minimum Based on Radiocarbon in Danish Oak. Geophysical Research Letters 46, 8617-8623.
#'
#' [29] Fogtmann-Schulz, A, Ostbo, SM, Nielsen, SGB, Olsen, J, Karoff, C, Knudsen, MF, 2017. Cosmic ray event in 994 C.E. recorded in radiocarbon from Danish oak. Geophysical Research Letters 44, 8621-8628.
#'
#' [30] Kudsk, S., B. Philippsen, C. Baittinger, A. Fogtmann-Schulz, M. Knudsen, C. Karoff, J. Olsen, 'New single-year radiocarbon measurements based on Danish oak covering the periods AD 692-790 and 966-1057 AD', (in press), Radiocarbon.
#'
#' [31] Manning, S. W. and B. Kromer (2012). 'Considerations of the Scale of Radiocarbon Offsets in the East Mediterranean, and Considering a Case for the Latest (Most Recent) Likely Date for the Santorini Eruption.' Radiocarbon 54(3-4): 449-474.
#'
#' [32] Manning, S.W., Griggs, C., Lorentzen, B., Bronk Ramsey, C., Chivall, D., Jull, A.J.T., Lange, T.E. 2018. Fluctuating Radiocarbon Offsets Observed in the Southern Levant and Implications for Archaeological Chronology Debates. Proceedings of the National Academy of Sciences of the United States of America 115:6141-6146.
#'
#' [33] Dellinger, F, Kutschera, W, Nicolussi, K, Schiessling, P, Steier, P, and Wild, E M, 2004 A 14C calibration with AMS from 3500 to 3000 BC, derived from a new high-elevation stone-pine tree-ring chronology, Radiocarbon, 46, 969-83
#'
#' [34] Steier, P, Dellinger, F, Kutschera, W, Priller, A, Rom, W, and Wild, E M, 2004 Pushing the precision limit of 14C AMS, Radiocarbon, 46, 5-17
#'
#' [35] Ozaki, H, Imamura, M, Matsuzaki, H, Mitsutani, T, 2007. Radiocarbon in 9th to 5th century BC tree-ring samples from the Ouban 1 archaeological site, Hiroshima, Japan. Radiocarbon 49, 473-479.
#'
#' [36] The origin of the farming in the Yayoi Period and East Asia: Establishment of High-Precision Chronology by carbon 14 age analysis.  National Museum of Japanese History Edited by Toyohiro Nishimoto, 524p, 2009. (in Japanese, final progress report of JSPS Grant-in-Aids (16GS0118))
#'
#' [37] Sakamoto, M, Imamura, M, van der Plicht, J, Mitsutani, T, Sahara, M, 2003. Radiocarbon calibration for Japanese wood samples. Radiocarbon 45, 81-89.
#'
#' [38] Okuno, M, Hakozaki, M, Miyake, F, Kimura, K, Masuda, K, Sakamoto, M, Hong, W, Yatsuzuka, S, Nakamura, T, 2018. Chronological significance of d14C spike and precise age determination of the B-Tm Tephra, China/ North Korea, 23rd International Radiocarbon Conference, Trondheim, Norway.
#'
#' [39] Sakamoto, M, Hakozaki, M, Nakao, N, Nakatsuka, T, 2017. Fine structure and reproducibility of radiocarbon ages of middle to early modern Japanese tree rings. Radiocarbon 59, 1907-1917.
#'
#' [40] Jull, AT, Panyushkina, I, Miyake, F, Masuda, K, Nakamura, T, Mitsutani, T, Lange, TE, Cruz, RJ, Baisan, C, Janovics, R, 2018. More Rapid 14 C Excursions in the Tree-Ring Record: A Record of Different Kind of Solar Activity at About 800 BC? Radiocarbon 60, 1237-1248.
#'
#' [41] Miyake F., Jull A.J.T., Panyushkina I.P., Wacker L., Salzer M., Baisan C., Lange T., Cruz R., Masuda K., Nakamura T. 2017. Large 14C excursion in 5480 BC indicates an abnormal sun in the mid-Holocene. PNAS Physical Sciences - Earth, Atmospheric, and Planetary Sciences 114 (3), doi:10.1073/pnas.161314411
#'
#' [42] Pearson, C.L., Brewer, P.W., Brown, D., Heaton, T.J., Hodgins, G.W., Jull, A.T., Lange, T. and Salzer, M.W., (2018). Annual radiocarbon record indicates 16th century BCE date for the Thera eruption. Science advances, 4(8), p.eaar8241.
#'
#' [43] Pearson, CL, Wacker, L, Bayliss, A, Brown, DM, Salzer, M, Brewer, PW, Bollhalder, S, Boswijk, G, Hodgins, GWL, Annual variation in atmospheric 14C between 1700 BC and 1480 BC: Radiocarbon: this issue
#'
#' [44] Jull A.J.T., Panyushkina I.P., Lange T.E., Kukarskih V.V., Clark K.J., Myglan V.S., Salzer M., Burr G.S., Leavitt S.L.  Excursions in the 14C record at AD 774-775 from tree rings from Russia and America.2014. Geophysical Research Letters 41 (8): 3004-3010.  10.1002/2014GL059874
#'
#' [45] Miyake F., Masuda K., Nakamura T., Kimura K., Hakozaki M., Jull A.T., Lange T., Cruz R., Panyushkina I.P., Baisan C., Salzer M. 2017. Search for annual 14C excursions in the past. Radiocarbon 59 (2): 315-320. DOI: 10.1017/RDC.2016.54
#'
#' [46] Wacker et al. in prep
#'
#' [47] Sookdeo, A, Kromer, B, Adolphi, F, Beer, J, Brehm, N, Buntgen, U, Christl, M, Eglinton, TI, Friedrich, M, Guidobaldi, G, Helle, G, Nievergelt, D, Pauly, M, Reinig, F, Tegel, W, Treydte, K, Synal, H-A, Wacker, L, submitted. There Goes the Sun: 14C in trees reveals reduced solar activity during the Younger Dryas. Nature Geoscience.
#'
#' [48] Buntgen, Ulf, et al, (2018),'Tree rings reveal globally coherent signature of cosmogenic radiocarbon events in 774 and 993 CE'. Nature Communications 9, 3605
#'
#' [49] Bayliss et al. in prep
#'
#' [50] Adolphi, F., R. Muscheler, M. Friedrich, D. Guttler, L. Wacker, S. Talamo and B. Kromer (2017). 'Radiocarbon calibration uncertainties during the last deglaciation: Insights from new floating tree-ring chronologies.' Quaternary Science Reviews 170: 98-108.
#'
#' [51] Turney CS, Palmer J, Ramsey CB, Adolphi F, Muscheler R, Hughen KA, Staff RA, Jones RT, Thomas ZA, Fogwill CJ. 2016. High-precision dating and correlation of ice, marine and terrestrial sequences spanning Heinrich Event 3: Testing mechanisms of interhemispheric change using New Zealand ancient kauri (Agathis australis). Quaternary Science Reviews 137:126-34.
#'
#' [52] Turney, C.S.M., Fifield, L.K., Hogg, A.G., Palmer, J.G., K., H., Baillie, M.G.L., Galbraith, R., Ogden, J., Lorrey, A., Tims, S.G., Jones, R.T., 2010. Using New Zealand kauri (Agathis australis) to test the synchronicity of abrupt climate change during the Last Glacial Interval (60,000-11,700 years ago). Quaternary Science Reviews 29, 3677-3682.
#'
#' [53] Bronk Ramsey, C, Staff, RA, Bryant, CL, Brock, F, Kitagawa, H, van der Plicht, J, Schlolaut, G, Marshall, MH, Brauer, A, Lamb, HF, Payne, RL, Tarasov, PE, Haraguchi, T, Gotanda, K, Yonenobu, H, Yokoyama, Y, Tada, R, Nakagawa, T, 2012. A Complete Terrestrial Radiocarbon Record for 11.2 to 52.8 kyr BP. Science 338, 370-374.
#'
#' [54] Gordon Schlolaut, Richard A Staff, Michael H Marshall, Achim Brauer, Christopher Bronk Ramsey, Henry F Lamb, Takeshi Nakagawa, 2018, An extended and revised Lake Suigetsu varve chronology from ~50 to ~10 ka BP based on detailed sediment micro-facies analyses, Quaternary Science Reviews 200, 351-366
#'
#' [55] Bard, E, Hamelin, B, Fairbanks, RG, Zindler, A. 1990. Calibration of the 14C timescale over the past 30,000 years using mass spectrometric U-Th ages from Barbados corals. Nature 345: 405-410.
#'
#' [56] Bard, E, Arnold, M, Hamelin, B, Tisnerat-Laborde, N, Cabioch, G, 1998. Radiocarbon calibration by means of mass spectrometric Th- 230/U-234 and C-14 ages of corals: An updated database including samples from Barbados, Mururoa and Tahiti. Radiocarbon 40, 1085-1092.
#'
#' [57] Fairbanks, RG, Mortlock, RA, Chiu, TC, Cao, L, Kaplan, A, Guilderson, TP, Fairbanks, TW, Bloom, AL, Grootes, PM & Nadeau, MJ. 2005. Radiocarbon calibration curve spanning 0 to 50,000 years BP based on paired Th-230/U-234/U-238 and C-14 dates on pristine corals. Quaternary Science Reviews 24(16-17): 1781-96.
#'
#' [58] Cutler, KB, Gray, SC, Burr, GS, Edwards, RL, Taylor, FW, Cabioch, G, Beck, JW, Cheng, H, and Moore, J. 2004. Radiocarbon calibration to 50 kyr BP with paired 14C and 230Th dating of corals from Vanuatu and Papua New Guinea. Radiocarbon 46: 1127-1160.
#'
#' [59] Hughen, KA, Southon, JR, Lehman, SJ, Overpeck, JT, 2000. Synchronous radiocarbon and climate shifts during the last deglaciation. Science 290, 1951-1954.
#'
#' [60] Hughen, KA, Southon, JR, Bertrand, CJH, Frantz, B, Zermeno, P. 2004. Cariaco Basin calibration update: revisions to calendar and 14C chronologies for core PL07-58PC. Radiocarbon 46: 1161-1187.
#'
#' [61] Durand, N, Deschamps, P, Bard, E, Hamelin, B, Camoin, G, Thomas, AL, Henderson, GM, Yokoyama, Y, Matsuzaki, H. 2013.  Comparison of 14C and U-Th in corals from IODP #310 cores offshore Tahiti.  Radiocarbon 55 (4), 1947-1974.
#'
#' [62] Hughen, K, Southon, J, Lehman, S, Bertrand, C, Turnbull, J, 2006. Marine-derived 14C calibration and activity record for the past 50,000 years updated from the Cariaco Basin. Quaternary Science Reviews 25, 3216-3227.
#'
#' [63] Hughen, K, Heaton, TJ. Updated Cariaco Basin 14C Calibration Dataset from 0-60k BP, in prep
#'
#' [64] Bard, E, Rostek, F, Menot-Combes, G, 2004. Radiocarbon calibration beyond 20,000 14C yr B.P. by means of planktonic foraminifera of the Iberian Margin. Quaternary Research 61, 204-214.
#'
#' [65] Edouard Bard, Guillemette Menot, Frauke Rostek, Laetitia Licari, Philipp Boening,R Lawrence Edwards, Hai Cheng, Yongjin Wang, Timothy J Heaton, (2013) 'Radiocarbon calibration/comparison records based on marine sediments from the Pakistan and Iberian margins', Radiocarbon, Vol 55, Nr 4, 2013, p 1999-2019
#'
#' [66] Southon J, Noronha AL, Cheng H, Edwards RL, Wang YJ. (2012). A high-resolution record of atmospheric C-14 based on Hulu Cave speleothem H82. Quaternary Science Reviews 33:32-41
#'
#' [67] Cheng H, Edwards RL, Southon J, Matsumoto K, Feinberg JM, Sinha A, Zhou W, Li H, Li X, Xu Y. 2018. Atmospheric 14C/12C changes during the last glacial period from Hulu Cave. Science 362(6420):1293-7
#'
#' [68] Dirk L. Hoffmann, J. Warren Beck, David A. Richards, Peter L. Smart, Joy S. Singarayer, Tricia Ketchmark, Chris J. Hawkesworth. 2010. Towards radiocarbon calibration beyond 28 ka using speleothems from the Bahamas,Earth and Planetary Science Letters,289:1-10.
#'
#' [69] J. Warren Beck, David A. Richards, R. Lawrence Edwards, Bernard W. Silverman, Peter L. Smart, Douglas J. Donahue, Sofia Hererra-Osterheld, George. S. Burr, Leal Calsoyas, A. J. Timothy Jull, Dana Biddulph. 2001. Extremely Large Variations of Atmospheric 14C Concentration During the Last Glacial Period Science 292:2453
#'
#' [70] Vogel et al. 1993. Pretoria calibration curve for short-lived samples, 1930-3350 BC. Radiocarbon 35: 73-85.
#'
#' [71] Stuiver, Braziunas 1998. Anthropogenic and solar components of hemispheric 14C. Geophysical Research Letters 25: 329-332.
#'
#' [72] Hogg et al. 2002 High-precision radiocarbon measurements of contemporaneous tree-ring dated wood from the British Isles and New Zealand: AD 1850-950. Radiocarbon 44: 633-640.
#'
#' [73] McCormac et al. 1998. Temporal variation in the interhemispheric C-14 offset. Geophysical Research Letters 25: 1321-1324.
#'
#' [74] Hogg et al. 2011 High-precision radiocarbon measurements of tree-ring dated wood from New Zealand: 195 BC-AD 995. Radiocarbon 53, 3: 529-542.
#'
#' [75] Hogg et al. 2013 Is there any evidence for regional 14C offsets in the Southern Hemisphere? \doi{10.2458/azu_js_rc.v55i2.16104}
#'
#' [76] Hogg et al. 2002 High-precision radiocarbon measurements of contemporaneous tree-ring dated wood from the British Isles and New Zealand: AD 1850-950. Radiocarbon 44: 633-640.
#'
#' [77] McCormac et al. 1998. Temporal variation in the interhemispheric C-14 offset. Geophysical Research Letters 25: 1321-1324.
#'
#' [78] Hua et al. 2009 Atmospheric 14C variations derived from tree rings during the early Younger Dryas. Quaternary Science Reviews, v. 28, 25-26: 2982-2990.
#'
#' [79] Hua et al. 2004 Radiocarbon in tropical tree rings during the Little Ice Age. Nuclear Instruments and Methods in Physics Research B 223-224:489-94.
#'
#' [80] Hogg et al. 2013 SHCal13 Southern Hemisphere calibration, 0-50,000 cal yr BP. Radiocarbon 55, 2
#'
#' [81] Zimmerman et al. 2010 Extension of the Southern Hemisphere atmospheric radiocarbon curve, 2120-850 years BP: results from Tasmanian huon pine. Radiocarbon 52, 203: 887-94.
#'
#' [82] Boentgen et al. 2018 Tree rings reveal globally coherent signature of cosmogenic radiocarbon events in 774 and 993 CE. Nature Communications, 9: 3605. doi:10.1038/s41467-018-06036-0.
#' [83] Sookdeo et al. 2020 Quality Dating: A well-defined protocol implemented at ETH Zurich for high-precision 14C dates tested on Late Glacial wood. Radiocarbon. \doi{10.1017/RDC.2019.132}
#' @export
intcal.data <- function(cal1, cal2, cc1="IntCal20", cc2=NA, calcurve.data="IntCal20", select.sets=c(), realm="C14", BCAD=FALSE, cal.lab=NA, cal.rev=FALSE, c14.lab=NA, c14.lim=NA, c14.rev=FALSE, ka=FALSE, cc1.col=rgb(0,0,1,.5), cc1.fill=rgb(0,0,1,.2), cc2.col=rgb(0,.5,0,.5), cc2.fill=rgb(0,.5,0,.2), data.cols=c(), data.pch=c(1,2,5,6,15:19), pch.cex=.5, legend.loc="topleft", legend.ncol=2, legend.cex=0.7, cc.legend="bottomright", bty="l",  ...) {

  # read the data
  if(tolower(calcurve.data) == "intcal20") {
    dat <- system.file("extdata/", "intcal20_data.txt", package = "rintcal")
    sourcesdat <- system.file("extdata/", "intcal20_data_sources.txt", package = "rintcal")
    sourcesdat <- fastread(sourcesdat, nrows=32, sep=",")
  }
  if(tolower(calcurve.data) == "shcal20") {
    dat <- system.file("extdata/", "shcal20_data.txt", package = "rintcal")
    sourcesdat <- system.file("extdata/", "shcal20_data_sources.txt", package = "rintcal")
    sourcesdat <- fastread(sourcesdat, nrows=9, sep=",")
  }
  dat <- fastread(dat, header=TRUE, sep=" ")

  if(BCAD)
    dat$cal <- 1950 - dat$cal

  # find the data corresponding to the period of interest
  if(BCAD) {
    mindat <- dat$cal >= min(cal1, cal2)*1.01 # adding some extra space
    maxdat <- dat$cal <= max(cal1, cal2)/1.01 # adding some extra space
  } else {
     mindat <- dat$cal >= min(cal1, cal2)/1.01 # adding some extra space
     maxdat <- dat$cal <= max(cal1, cal2)*1.01 # adding some extra space
  }
  
  dat <- dat[which( mindat * maxdat == 1),]
  if(length(select.sets) > 0)
    dat <- dat[which(dat$set %in% select.sets),]

  # read and narrow down the calibration curve(s)
  cc.1 <- ccurve(cc1)
  if(BCAD) {
    cc.1[,1] <- 1950 - cc.1[,1]
    mindat <- cc.1[,1] >= (min(cal1, cal2)*1.01) # adding some extra space
    maxdat <- cc.1[,1] <= (max(cal1, cal2)/1.01) # adding some extra space
  } else {
      mindat <- cc.1[,1] >= (min(cal1, cal2)/1.01) # adding some extra space
      maxdat <- cc.1[,1] <= (max(cal1, cal2)*1.01) # adding some extra space
  }
  cc.1 <- cc.1[which(mindat * maxdat == 1),]
  if(ka)
    cc.1 <- cc.1/1e3

  # deal with different C14 'realms' (radiocarbon age, F14C, pMC or D14C)
  if("f" %in% tolower(realm)) {
    F <- C14.F14C(cc.1[,2], cc.1[,3])
    cc.1[,2:3] <- F
    F <- C14.F14C(dat$c14, dat$c14sig)
    dat$c14 <- F[,1]
    dat$c14sig <- F[,2]
  }
  if("p" %in% tolower(realm)) {
    p <- C14.pMC(cc.1[,2], cc.1[,3])
    cc.1[,2:3] <- p
    p <- C14.pMC(dat$c14, dat$c14sig)
    dat$c14 <- p[,1]
    dat$c14sig <- p[,2]
  }
  if("d" %in% tolower(realm)) {
    F <- C14.F14C(cc.1[,2], cc.1[,3])
    Dmax <- F14C.D14C(F[,1]+F[,2], cc.1[,1])
    D <- F14C.D14C(F[,1], cc.1[,1])
    Dsd <- Dmax - D
    cc.1[,2:3] <- cbind(D, Dsd)

    F <- C14.F14C(dat$c14, dat$c14sig)
    Dmax <- F14C.D14C(F[,1]+F[,2], dat$cal)
    D <- F14C.D14C(F[,1], dat$cal)
    dat$c14 <- D
    dat$c14sig <- Dmax - D
  }

  cc1.pol <- cbind(c(cc.1[,1], rev(cc.1[,1])), c(cc.1[,2]-cc.1[,3], rev(cc.1[,2]+cc.1[,3])))

  if(!is.na(cc2)) {
    cc.2 <- ccurve(cc2)
    if(BCAD)
      cc.2[,1] <- 1950 - cc.2[,1]
    mindat <- cc.2[,1] >= min(cal1, cal2)
    maxdat <- cc.2[,1] <= max(cal1, cal2)

    if("f" %in% tolower(realm)) {
      F <- C14.F14C(cc.2[,2], cc.2[,3])
      cc.2[,2:3] <- F
    }
    if("p" %in% tolower(realm)) {
      p <- C14.pMC(cc.2[,2], cc.2[,3])
      cc.2[,2:3] <- p
    }
    if("d" %in% tolower(realm)) {
      F <- C14.F14C(cc.2[,2], cc.2[,3])
      Dmax <- F14C.D14C(F[,1]+F[,2], cc.2[,1])
      D <- F14C.D14C(F[,1], cc.2[,1])
      cc.2[,2:3] <- cbind(D, Dmax-D)
    }
    cc.2 <- cc.2[which(mindat * maxdat == 1),]
    if(ka)
      cc.2 <- cc.2/1e3
    cc2.pol <- cbind(c(cc.2[,1], rev(cc.2[,1])), c(cc.2[,2]-cc.2[,3], rev(cc.2[,2]+cc.2[,3])))
  }

  # different datasets need different colours and symbols
  sets <- dat$set
  set.cols <- NA; set.pchs <- NA
  these.sets <- sort(unique(sets))
  # but possibly reorder if important dataset, e.g. Suigetsu...

  if(length(data.cols) == 0)
    if(length(these.sets) > 8) {
    # then add 16 distinct colours (taken from the randomcoloR package)
    data.cols <- c(1:8, "#739B7E", "#DCA97C", "#D3DECE", "#D19CE2", "#DB58BB",
    "#DDC3D4", "#B042E4", "#DB6240", "#CD788F", "#7972D3", "#D2E49D",
    "#87EB55", "#7FB0D9", "#77E299", "#DAD950", "#80E6DC")
    data.pch <- rep(data.pch, 5)
    } else
      data.cols <- 1:8
    these.cols <- data.cols[1:length(sets)]
    these.pchs <- data.pch[1:length(sets)]
  for(i in 1:length(these.sets)) {
    set.cols[sets %in% these.sets[i]] <- these.cols[i]
    set.pchs[sets %in% these.sets[i]] <- these.pchs[i]
  }

  # set up the plot parameters
  cal <- dat$cal
  cal.err <- dat$calsig
  cal.err[which(cal.err == 1)] <- 0 # do not plot yearly errors

  # dendro-dated wood (IntCal20 datasets 1 to 69) has assumed 0-yr calendar age errors, and the error bars reflect the amount of rings in the wood blocks (e.g., 10-year, 1-year)
  if(calcurve.data == "IntCal20") {
    cal.err[which(sets <= 69)] <- cal.err[which(sets <= 69)]/2
  } else
      cal.err <- cal.err/2 # SH data are all tree-rings
  c14 <- dat$c14
  c14.err <- dat$c14sig

  if(is.na(cal.lab))
    callab <- "cal. yr BP" else
      callab <- cal.lab
  if(is.na(c14.lab))
    if("p" %in% tolower(realm))
      c14lab <- "pMC" else
        if("f" %in% tolower(realm))
          c14lab <- expression(F^14*C) else
            if("d" %in% tolower(realm))
              c14lab <- expression(delta^14*C) else
                c14lab <- expression(""^14*C~BP)

  cal.lim <- c(cal1, cal2)
  if(cal.rev)
    cal.lim <- rev(cal.lim)

  if(BCAD) {
 #   cal <- 1950-cal
    if(is.na(cal.lab))
      callab <- "BC/AD"
  }

  if(ka) {
    cal <- cal/1e3
    cal.err <- cal.err/1e3
    c14 <- c14/1e3
    c14.err <- c14.err/1e3
    if(is.na(c14.lab))
      c14lab <- expression(""^14*C~kBP)
    if(is.na(cal.lab))
      callab <- ifelse(BCAD, "kcal BC/AD", "kcal BP")
    cal.lim <- cal.lim/1e3
  }

  if(is.na(c14.lim)[1])
    if(is.na(cc2))
      c14.lim <- range(c14-c14.err, c14+c14.err, cc1.pol[,2]) else
        c14.lim <- range(c14-c14.err, c14+c14.err, cc1.pol[,2], cc2.pol[,2])
  if(c14.rev)
    c14.lim <- rev(c14.lim)

  # draw the graph and data
  plot(0, type="n", xlim=cal.lim, xlab=callab, ylim=c14.lim, ylab=c14lab, bty=bty, ...)
  points(cal, c14, col=set.cols, pch=set.pchs, cex=pch.cex) # data points
  segments(cal, c14-c14.err, cal, c14+c14.err, col=set.cols) # c14 errors
  segments(cal-cal.err, c14, cal+cal.err, c14, col=set.cols) # cal errors

  # add the calibration curve(s)
  polygon(cc1.pol, col=cc1.fill, border=NA) # calibration curve
  lines(cc.1[,1], cc.1[,2]-cc.1[,3], col=cc1.col)
  lines(cc.1[,1], cc.1[,2]+cc.1[,3], col=cc1.col)
  if(!is.na(cc2)) {
    polygon(cc2.pol, col=cc2.fill, border=NA) # calibration curve
    lines(cc.2[,1], cc.2[,2]-cc.2[,3], col=cc2.col)
    lines(cc.2[,1], cc.2[,2]+cc.2[,3], col=cc2.col)
  }

  # legend
  if(!is.na(legend.loc))
  if(length(cal) > 1) { # do not bother if there are no data to show
    set <- sourcesdat[which(sourcesdat[,1] %in% these.sets),2]
    legend(legend.loc, set, col=these.cols, pch=these.pchs, cex=legend.cex, ncol=legend.ncol, text.col=these.cols, bty="n")
  }

  if(!is.na(cc.legend)) {
    if(cc1 == 1)
      nm <- "IntCal20" else
        if(cc1 == 2)
          nm <- "Marine20" else
            if(cc1 == 3)
              nm <- "SHCal20" else
                nm <- cc1
    if(is.na(cc2)) {
      cc.col <- cc1.col
    } else {
      if(cc2 == 1)
        nm2 <- "IntCal20" else
          if(cc2 == 2)
            nm2 <- "Marine20" else
              if(cc2 == 3)
                nm2 <- "SHCal20" else
                  nm2 <- cc2
        nm <- c(nm, nm2)
        cc.col <- c(cc1.col, cc2.col)
      }
    legend(cc.legend, legend=nm, text.col=cc.col, cex=legend.cex, bty="n", xjust=0)
  }
  invisible(dat)
}

