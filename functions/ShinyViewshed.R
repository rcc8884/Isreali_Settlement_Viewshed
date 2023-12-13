Rver <- R.version
if (as.numeric(Rver$major)<4)
  stop(sprintf("The package requires version 4.0.0 or later. You have %s installed, please update !!",Rver$version.string))

InstallSourcePcks <- function(pcks){
  for( i in pcks ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      print(sprintf("Package %s not installed, downloading and installing it now!",i))
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

pcks <- list("dplyr","lubridate","leaflet","leaflet.extras",
             "htmltools","doParallel",
             "raster","tiff","sp","shiny","rsconnect", "png")
InstallSourcePcks(pcks)

wgs84 <- "+proj=longlat +datum=WGS84"
Point <<- data.frame(LAT= numeric(0), LON= numeric(0))
earthRadius <- 6371000

linear <- function (x, observer, target,lineLength) 
{
  v <- observer - target
  y <- ((x - observer[1])/v[1])*v[2]+observer[2]
  z <- ((x - observer[1])/v[1])*v[3]+observer[3]
  dist <- -((x - observer[1])/v[1])*lineLength
  data.frame(x=x,y=y, z=z,dist=dist)
}

myTransperent <- rgb(0,0,0,alpha=0,maxColorValue = 255)
mygreen <- rgb(0,255,0,alpha=200,maxColorValue = 255)

myAnd <- function(a_list,some_indeces){
  if (length(some_indeces)==0)
  {return(a_list[[1]]==a_list[[1]])
    Break}
  AndVals <- a_list[[some_indeces[1]]]
  for (i in 1:length(some_indeces))
  {
    AndVals <- a_list[[some_indeces[i]]]&AndVals
  }
  return(AndVals)
}

myNot <- function(a_list,some_indeces){
  if (length(some_indeces)==0)
  {return(a_list[[1]]==a_list[[1]])
    Break}
  AndVals <- !a_list[[some_indeces[1]]]
  for (i in 1:length(some_indeces))
  {
    AndVals <- AndVals&!a_list[[some_indeces[i]]]
  }
  return(AndVals)
}
myAnd2 <- function(LOSLayer1,LOSLayer2)  
{
  r <- stack(LOSLayer1,LOSLayer2)
  return(r[[1]]&r[[2]])
}

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
N_colors <- 9;
cpal_C <- c(grey.colors(2,start = 0.8,end=1),rainbow(4, start = 0, end =0.2,rev=T),rep("#B800FF",4))
val_C = as.numeric(1:length(cpal_C))
pal_C = colorNumeric(cpal_C,val_C, na.color = "transparent")

cpal_I <- c("transparent","yellow")
val_I = as.numeric(0:1)
pal_I = colorNumeric(cpal_I,val_I, na.color = "transparent")

cpal_inv <- c("transparent","black")
val_inv = as.numeric(0:1)
pal_inv = colorNumeric(cpal_inv,val_inv, na.color = "transparent")

LOSfilesnames <- dir("LOSData")
LOSfilesnames <- LOSfilesnames[grep("_vsf.Rdata",LOSfilesnames)]
LOSfilesnames <- gsub("_vsf.Rdata","",LOSfilesnames)
IconsVis <- iconList(
  #NoSeeIcon = makeIcon("LOSData/house.png", iconWidth = 15, iconHeight = 15),
  SeeIcon = makeIcon("LOSData/Green-Circle.png", iconWidth = 8, iconHeight = 8))


# Define the coverage function
coverage <- function(image_path) {
  if (!file.exists(image_path)) {
    stop("The file does not exist at the specified path: ", image_path)
  }
  
  img <- readPNG(image_path)
  
  # If img contains an alpha channel, remove it
  if (dim(img)[3] == 4) {
    img <- img[,,1:3]
  }
  
  # Rescale color values to 0-255 if they're between 0 and 1
  if (isTRUE(all(img <= 1))) {
    img <- img * 255
  }
  
  # Convert the image to a data frame
  img_data <- data.frame(
    r = as.vector(img[,,1]),
    g = as.vector(img[,,2]),
    b = as.vector(img[,,3])
  )
  
  # Define the average colors for purple and green based on previous analysis
  average_purple <- c(151, 93, 160)
  average_green <- c(55, 126, 34)
  
  tolerance <- 50
  
  # Function to create a mask based on color tolerance
  color_mask <- function(color_data, target_color, tolerance) {
    r_within <- abs(color_data$r - target_color[1]) < tolerance
    g_within <- abs(color_data$g - target_color[2]) < tolerance
    b_within <- abs(color_data$b - target_color[3]) < tolerance
    return(r_within & g_within & b_within)
  }
  
  purple_mask <- color_mask(img_data, average_purple, tolerance)
  green_mask <- color_mask(img_data, average_green, tolerance)
  
  purple_coverage <- mean(purple_mask)
  green_coverage <- mean(green_mask)
  combined_mask <- purple_mask | green_mask
  combined_coverage <- mean(combined_mask)
  
  #cat("Percentage of image that is the specific shade of purple:", purple_coverage * 100, "%\n")
  #cat("Percentage of image that is the specific shade of green:", green_coverage * 100, "%\n")
  cat("Percentage of image that is either shade:", combined_coverage * 100, "%\n")
}


ui = fluidPage(
  tabsetPanel( 
    tabPanel("Welcome",
             fluid = TRUE,
             div(
               class = "custom-text-container",
               
               # Top row with h2 and h3 elements
               div(
                 style = "text-align: center; margin-bottom: 30px;",  # Centering and adding space
                 h3("Strategic Placement of Israeli Settlements in the West Bank"),
                 h4("Richard Carter IV, Dr. Laurie Heyer, Dr. Jakub Kabala"),
                 h4("Davidson College: Fall 2023"),
               ),
               
               div(style = "margin-bottom:20px;",
                 h3("Contents"),
               ),
               
               # Two-column layout for the rest of the content
               div(
                 style = "display: flex; justify-content: space-between;",  # Flexbox for two-column layout
                 
                 div(
                   style = "width: 26%;",  # First column
                   h4("Behind the Project"),
                   h4("Historical Context"),
                   p("  - Israel's Beginnings"),
                   p("  - First 20 Years"),
                   p("  - Six-Day War and the First Settlements"),
                   p("  - Lead-up to the Yom Kippur War"),
                   p("  - Israel's New Focus"),
                   p("  - First Intifada and a Growing Settlement Population"),
                   p("  - The Oslo Accords"),
                   p("  - Lead-up and The Second Intifada"),
                   p("  - The Rise of Hamas"),
                   p("  - Internal Palestinian Conflicts and Gaza War"),
                   p("  - 2008-2014"),
                   p("  - 2014 Gaza War to 2020"),
                   p("  - 2021 Conflict in Jerusalem and Present Day"),
                   h4("Brief History of Israeli Settlements"),
                   p("  - Israeli Settlement Overview"),
                   p("  - Early Settlements"),
                   p("  - A New Agenda"),
                   p("  - Current Day"),
                   h4("Viewshed Instructions"),
                   p("  - Loading the Data"),
                   p("  - Running the Cumulative Viewshed"),
                   h4("Load Data"),
                   h4("Cumulative Viewshed"),
                   h4("Viewshed Analysis"),
                   p("Percent Seen by Settlements"),
                   p("Major Cities"),
                   p("Populated Tan Areas"),
                   p("Southeast West Bank"),
                   p("Conclusion"),
                   h4("Works Cited")
                 ),
                 
                 div(
                   style = "width: 70%;",  # Adjust as needed
                   imageOutput("home_img")
                 ),
               )
             )
    ),
    tabPanel("Behind the Project",
             fluid = TRUE,
             tags$div(
               class = "custom-text-container",
               
               h3("Behind the Project"),
               
               p("At the beginning of this project, I was interested in studying the Holy City of Jerusalem, a centerpiece for 
                 the Abrahamic religions. It is the place where Abraham almost sacrificed his son, the City of David, and is 
                 the city at the center of the Jewish faith. For Christians, it is the place where Jesus was crucified and 
                 resurrected with the Bible placing Jerusalem at the center of the end times with a new Jerusalem coming down 
                 from heaven. In the Quran, it is the location of Muhammad's night journey and ascension to heaven and was the 
                 first Qibla or direction of prayer. With the city being the most important place for Judaism and Christianity 
                 and the third holiest in Islam, there has been constant unrest within the city ever since David in roughly 1000 BC. 
                 However, even with the tensions and constant fighting, it has also seen the three religions show great cooperation 
                 with each other. For example, the keys to the holiest site in Christianity, the Church of the Holy Sepulchre, have 
                 been held by the same Muslim family since the 7th century. Overall, I had a high interest in the city."),
               
               p("At the beginning of the semester, I started by gathering data from the Jerusalem Statistical Yearbook and the
                 Palestinian Statistical Yearbook, believing I would look at the differences in life in Jerusalem, Israel, and the
                 West Bank. However, after learning about viewshed analysis and reading Occupation, Sight, Landscape: 
                 Visibility and the Normalization of Israeli Settlements by Jakub Zahora, the project quickly changed. In a paper
                 talking about settler's perspectives on the settlements, one of the key sticking points is how beautiful the scenery
                 is. He argues this is mainly due to the topographical prominence of settlements since they are often located on hilltops
                 that dominate the given area, overlooking Palestinian cities and villages in the valleys. In Eyal Weizman's book 
                 Hollow Land, he calls this idea “the politics of verticality,” continuing that settlements are given their strategic 
                 importance by “establishing control over the Palestinian villages located below them, posing as points of surveillance 
                 over the Palestinian population.” Weizman continues and mentions how this is hidden from settlers who see the
                 “admiration of the rustic panorama” while the government can surveil. On a sunny day, one settler says he can
                 see a major Palestinian city over 10 miles away from his house."),
               
               p("For my project, I wanted to see if these claims of verticality are quantitative instead of theorizing the idea
                 that settlements are built in strategic places to overlook Palestinian villages. To test the theory and add to
                 the ongoing conversation, I first got four Digital Elevation Maps (DEM) from NASA and combined them using QGIS, 
                 covering the whole West Bank. Next, I found ViewshedR, an open-source tool on GitHub created by Eitam Arnon, 
                 Assaf Uzan, Michal Handel, Shlomo Cain, Sivan Toledo, and Orr Spiegel in June 2023. They created this free 
                 Viewshed application for R, taking a DEM map and providing line-of-sight (LOS) calculations, cumulative viewsheds,
                 and elevated-target analyses. While general Viewshed analysis has been available before using GIS software, this 
                 software can be restrictive, inaccessible, pricey, or difficult to use. For example, I looked into ArcGIS for this 
                 project, but it was too expensive. Meanwhile, QGIS was difficult to use. From here, I downloaded the ViewshedR and 
                 modified it for this project. With the original intention of ViewshedR looking at just a city basis, this would
                 be a significant technological processing problem."),
               
               p("After getting the basic skeleton for the code, the next step was gathering  the data. The Viewshed takes a name,
                 latitude and longitude coordinates, and a height. Since the number of settlements is highly contentious, I took an
                 official list of settlements published by the Israel Central Bureau of Statistics from 2016, the last year available, 
                 as the offical list. This method leads to an undercounting in settlements due to new settlement construction and some
                 settlements that exist but are not yet officially recognized by the Israeli government for one reason or another. 
                 Next, I took these settlement names and, using the Open Street Map API (OSM), pulled the borders of the settlements.
                 However, this only collected roughly half the settlements due to the other half not being on OSM . Therefore, I went
                 to the Google Earth API where I got most of the borders of the settlements I was missing. However, there were still
                 around ten settlements that were not recognized by Google Maps, leading me to search the internet for literature on
                 the settlements nearby area before personally marking out the settlement boundary. All that was left was the height.
                 I chose the height to be 2 meters (6 and a half feet) allowing the Viewshed to return all points you would be able
                 to see if an average person was standing on the ground (with a chair underneath them). While this may seem high, it 
                 would once again result in a underestimation of the visible landscape from the settlements. Settlement buildings,
                 especially those for potential surveillance purposes having multiple stories, leads to this underestimation. While
                 also considering 4 meters (13 feet) as another potential choice, in this instance, I think it's preferable to 
                 underestimate rather than overestimate."),
               
               p("After collecting all the data and changing the ViewshedR to work with my data, I began to test and fix errors. Here,
                 I fully realized the technology processing issue at hand. To run ViewshedR which breaks down the West Bank into 1/4th
                 mile squares, it would take 48 hours to run. Therefore, I decided to only use 25% of the data due to the border points
                 being close together, leading to the time it takes to compute being cut by 75%. However, I wanted more preciseness,
                 eventually settling on 1/10th mile squares, taking roughly five days to run on my computer. I looked into running it
                 through Jupyter notebook on a hosted server, but this was 3x slower than my computer. Since running the calculations
                 on a virtual network takes no processing power away from a computer, I decided to run both in case one failed. Four
                 days in, the virtual network failed, however, the one on my personal computer completed successfully after 110
                 hours. Since each square is 1/10th of a mile, in future Viewshed analysis, I would recommend running it on a 
                 higher resolution to get more precise results."),
               
               p("After running the code and working on the user interface, the last step was providing context and explaining what
                 occurs in the Viewshed. Overall, this project has been able to quantify claims presented about the geography and
                 location of Israeli settlements while informing viewers of the complicated history revolving around them. The 
                 Viewshed serves as a foundation, allowing for more extensive analyses in the future beyond what I could accomplish, 
                 as it merely marks the initial stage. In conclusion, this project began with a fascination for the Holy City of 
                 Jerusalem and evolved into an exploration of the strategic placement and visibility of Israeli settlements in the
                 West Bank. Through ViewshedR, the project quantitatively addressed the claims surrounding the geographical positioning 
                 of settlement locations, topography, and the surveillance of Palestinian areas. This endeavor contributes to the
                 ongoing discourse on the Israeli-Palestinian conflict and underscores the significance of advanced spatial analysis 
                 tools in understanding complex geopolitical landscapes.")
             )
    ),
    tabPanel("Historical Context",
             fluid = TRUE,
             tags$div(
               class = "custom-text-container",
               
               h3("Israel's Beginnings"),
               
               p("The modern state of Israel traces its origins to the late 19th century with the rise of Zionism, 
                 a movement advocating for the establishment of a Jewish state in the region of Palestine, then part 
                 of the Ottoman Empire. This aspiration was galvanized by growing anti-Semitism in Europe and the desire 
                 for a national homeland for the Jewish people. The Balfour Declaration of 1917, issued by the British 
                 government, supported the establishment of a national home for the Jewish people in Palestine. After 
                 World War II and the Holocaust, the urgency for a Jewish state increased, leading to the United Nations 
                 Partition Plan of 1947, which proposed dividing Palestine into Jewish and Arab states. Despite opposition 
                 and conflict, Israel declared its independence on May 14, 1948. This declaration marked the culmination 
                 of Zionist aspirations but also set the stage for ongoing and complex conflicts with the Arab world."),
               
               h3("First 20 Years"),
               
               p("Between 1948 and 1967, Israeli-Palestinian relations were marked by significant 
                 conflict and change. Following Israel's declaration of independence in 1948, neighboring Arab states 
                 launched a military offensive, leading to the Arab-Israeli War. Israel emerged victorious, but the war 
                 created a substantial Palestinian refugee problem and set the boundaries of the new state significantly 
                 beyond the UN's partition plan. During this period, Israel also witnessed significant numbers of Jewish 
                 immigrants. Tensions between Israel and its Arab neighbors, including the Palestinians, remained high, 
                 leading to sporadic violence and border conflicts. The Suez Crisis of 1956 further heightened tensions 
                 in the region. Palestinian nationalism began to grow, exemplified by the formation of groups like Fatah 
                 in 1959, which later played a significant role in the Palestinian struggle. This era culminated in the 
                 Six-Day War of 1967."),
               
               h3("Six-Day War and the First Settlements"),
               
               p("The Six-Day War in 1967 was a brief conflict fought between Israel and Egypt, Jordan, and Syria in 
                   June 1967. Israel's rapid and decisive victory in the war significantly altered the geopolitical landscape
                   of the region. The war led to Israel gaining control over the West Bank, Gaza Strip, Sinai Peninsula, 
                   and Golan Heights, territories that had previously been controlled by Jordan and Egypt. This led to 
                   the emergence of the first Israeli settlements in these areas, a development that would have profound 
                   and lasting implications for the region. In the beginning, the settlements were predominantly strategic, 
                   with the goal of establishing military strongholds. However, they quickly evolved to include civilian 
                   communities. The first of these were established in the Golan Heights and near Hebron in the West Bank. 
                   These first settlements were typically either agricultural communities inhabited by secular Labor Zionists, 
                   or by adherents of Religious Zionism, driven back to the Holy Lands with a religious purpose. However, 
                   these settlements are not beloved by all Israelis as critics see them as an obstacle to a peaceful solution. 
                   According to the Palestinian Central Bureau of Statistics, there are 719,452 Israeli settlers as of 2022."),
               
               h3("Lead-up to the Yom Kippur War"),
               
               p("Following the dramatic Israeli victory in 1967, the Arab states, primarily Egypt and Syria, sought to regain the territories 
                 lost to Israel. The Khartoum Resolution in 1967, with its Three No's - no peace with, no recognition of, and no negotiations 
                 with Israel - underscored the Arab world's stance post-1967. This period saw the War of Attrition (1967-1970) along the Suez 
                 Canal, where Egypt, engaged in sporadic, low-intensity conflict with Israel. Meanwhile, Israel fortified its positions in the 
                 occupied territories, further entrenching its presence. Superpowers got involved with the US supporting Israel while the Soviet 
                 Union deepened its involvement in the Palestine Liberation Organization (PLO), supplying their respective allies with military 
                 aid and diplomatic support. This raised concerns for Israel, which, despite its military prowess, became increasingly cognizant 
                 of the simmering tensions along its borders. This tension was for good reason as Egypt and Syria, which sought to reclaim the 
                 Sinai Peninsula and the Golan Heights, respectively, chose to attack Israel on Yom Kippur in 1973. The decision to launch the 
                 attack on October 6, 1973, Yom Kippur, the holiest day in the Jewish calendar, was strategic, aiming to catch Israel off-guard.
                 The war, though short, was intense and had significant implications for the region, eventually leading to a shift in the
                 geopolitical landscape and opening the door to future peace negotiations."),
               
               h3("Israel's New Focus"),
               
               p("The Yom Kippur War had a profound impact on Israel's sense of security, leading to a reassessment of 
                 its military and diplomatic strategies. Israel engaged in peace negotiations, culminating
                 in the historic Camp David Accords in 1978, which led to the peace treaty between Israel and Egypt 
                 in 1979. However, this did not directly address the Palestinian issue, leaving it as an ongoing conflict.
                 In the Palestinian territories, there was a growing sense of frustration and disenfranchisement. 
                 The Palestine Liberation Organization (PLO), led by Yasser Arafat, continued to gain international 
                 recognition as the representative body of the Palestinian people. Meanwhile, Israeli settlement activity 
                 in the occupied territories, including the West Bank and Gaza Strip, intensified during this period. 
                 The Israeli government, driven by a mix of security concerns and ideological motivations linked to the 
                 historic and religious significance of these lands to Judaism, encouraged the growth of Jewish settlements. 
                 This expansion was met with international criticism and was a source of heightened tension within the 
                 Palestinian population, who saw the settlements as an encroachment on their aspirations for statehood."),
               
               h3("First Intifada and a Growing Settlement Population"),
               
               p("During the 1980s, the Israeli settlement project in the occupied territories, especially in the West 
                 Bank and Gaza Strip, continued to expand with different Israeli governments, both from the right and 
                 the left, facilitating this growth to varying degrees. The settler population grew significantly, with 
                 the establishment of new settlements and the expansion of existing ones, often leading to tensions and 
                 clashes with the Palestinian population. This period also saw the rise of the Israeli settler movement, 
                 which became a powerful political force advocating for the expansion and permanence of these settlements.
                 On the Palestinian side, there was a growing resentment and frustration over the ongoing occupation and 
                 the expansion of Israeli settlements. This simmering discontent laid the groundwork for the First 
                 Intifada, or uprising, which erupted in 1987. 
                 
                 The First Intifada, a significant Palestinian uprising against Israeli occupation, erupted and continued until
                 the early 1990s. It began in the Gaza Strip and quickly spread to the West Bank. The Intifada was characterized
                 by widespread grassroots mobilization, with Palestinians engaging in demonstrations, strikes, boycotts, and civil
                 disobedience. The protests were often met with military and police responses that included the use of tear gas, rubber
                 bullets, and live ammunition, leading to significant casualties on the Palestinian side. The uprising also led to 
                 political changes. It strengthened the position of the Palestine Liberation Organization (PLO) as the 
                 representative of the Palestinian people and set the stage for future negotiations. The Intifada exposed 
                 the unsustainable nature of the occupation and highlighted the need for a political solution, eventually 
                 contributing to the Madrid Conference in 1991 and the subsequent Oslo Accords in 1993, which aimed to lay 
                 the groundwork for peace and the establishment of a Palestinian state. The international community, meanwhile, 
                 increasingly viewed the settlements as an obstacle to peace, with numerous United Nations resolutions 
                 condemning the expansion."),
               
               h3("The Oslo Accords"),
               
               p("The Oslo Accords, a landmark in the Israeli-Palestinian peace process, were a series of agreements negotiated between Israel 
                 and the Palestine Liberation Organization (PLO) in the early 1990s. The process began in secret negotiations in Oslo, Norway, 
                 culminating in the signing of the Oslo I Accord in 1993, famously marked by the handshake between Israeli Prime Minister Yitzhak 
                 Rabin and PLO Chairman Yasser Arafat on the White House lawn, overseen by U.S. President Bill Clinton. The Accords marked the 
                 first time Israel and the PLO officially recognized each other, with the PLO renouncing terrorism and recognizing Israel's right 
                 to exist, and Israel acknowledging the PLO as the legitimate representative of the Palestinian people. The Oslo Accords 
                 established a framework for future relations, aiming to achieve a peace treaty based on UN Resolutions 242 and 338. It set out a 
                 phased process for Palestinian self-governance in the West Bank and Gaza Strip over five years, with the intent to resolve key 
                 issues such as Israeli settlements, the status of Jerusalem, Palestinian refugees, and borders in subsequent negotiations. The 
                 Accords led to the establishment of the Palestinian Authority (PA), which was given limited self-governing powers in parts of 
                 the occupied territories. This led to the creation of Areas A, B, and C within the West Bank. Area A is administered by the PA, 
                 Area B is administered by the PA with shared security control with Israel, and Area C is administered by Israel. However, the 
                 Oslo Accords faced significant opposition from factions within both Israeli and Palestinian societies. Despite the initial hope 
                 and optimism, the Oslo process ultimately stalled, with fundamental issues unresolved, leading to a continued state of conflict 
                 and tension in the region. With the process stalling, the shift of the West Bank back to Palestinian control halted and reversed 
                 the trend with the continuation of Israeli settlements."),
               
               h3("Lead-up to the Second Intifada"),
               
               p("The period between the Oslo Accords in 1993 and the onset of the Second Intifada in 2000 was marked by both 
                  hope and increasing tension in the Israeli-Palestinian conflict. The assassination of Israeli Prime Minister 
                  Yitzhak Rabin in 1995 by a Jewish extremist opposed to the peace process was a significant setback. Rabin's 
                  successor, Benjamin Netanyahu, took a harder line on negotiations, slowing the peace process and expanding 
                  Israeli settlements in the occupied territories, which further exacerbated tensions. On the Palestinian side, 
                  frustration grew over the lack of progress in achieving statehood and improving living conditions. The 
                  Palestinian Authority faced internal challenges including accusations of corruption and failure to effectively 
                  govern. Meanwhile, extremist groups such as Hamas and Islamic Jihad carried out terrorist attacks against Israeli 
                  targets due to their disagreement with entering a peace process with Israel. The final years of the 1990s saw 
                  several attempts to revive the peace process, including the Wye River Memorandum in 1998 and the Camp David 
                  Summit in 2000. However, these efforts failed to resolve key issues. The visit of Israeli politician Ariel Sharon 
                  to the Temple Mount in September 2000, a site sacred to both Jews and Muslims, was a flashpoint and marked
                  the beginning of the Second Intifada. This period of relative calm but simmering discontent set the stage
                  for the eruption of a more violent and widespread conflict that would further entrench divisions and 
                  complicate the path to peace."),
               
               p("The Second Intifada, also known as the Al-Aqsa Intifada, erupted in late September 2000, triggered by Ariel 
                  Sharon's visit to the Temple Mount. This uprising was more violent and widespread than the first, lasting until 
                  around 2005. It began with widespread protests and riots but soon escalated into a deadly conflict, marked by 
                  suicide bombings, shootings, and other attacks by Palestinian militants against Israeli civilian and military
                  targets. In response, Israel launched extensive military operations in the West Bank and Gaza Strip, including
                  targeted killings, incursions into Palestinian areas, and strict movement restrictions on all Palestinians. The 
                  violence took a heavy toll on both sides although it was much heavier on the Palestinian side with 3,000 
                  Palestinian deaths estimated compared to 1,000 deaths for Israelis. The Second Intifada also helped contribute to
                  the rise of Islamist groups like Hamas and Islamic Jihad. The second Intifada led to increased security measures 
                  in Israel, including the construction of the West Bank barrier, which Israel argued was necessary for security 
                  but Palestinians viewed as a de facto annexation of territory. The Intifada exacerbated the divisions between 
                  them and made the prospects for a lasting peace more remote."),
               
               h3("The Rise of Hamas"),
               
               p("The Second Intifada gradually subsided around 2005 with the political landscape shifting significantly with the
                  democratic elections in the Palestinian territories in 2006. Hamas, an Islamist political and militant organization, 
                  won a surprising victory in these elections, leading to its control of the Palestinian Legislative Council. 
                  This victory was a significant milestone in the evolution of Hamas, which had been founded in the late 1980s and 
                  had grown in prominence during the Intifadas, primarily due to its social service networks and resistance to Israeli
                  occupation. Hamas ran in fully democratic elections on a platform of clean government, a thorough overhaul of the corrupt 
                 administrative system, and the issue of rampant lawlessness. Intent on reaching power by political means rather than by 
                 violence, Hamas announced it would refrain from attacks on Israel if Israel were to cease its offensives against Palestinian 
                 towns and villages while dropping the Islamic agenda and the claim to all of Palestine. After winning the 2006 election, 
                 Hamas offered Israel a ten-year truce in return for a complete Israeli withdrawal from the occupied Palestinian territories: 
                 the West Bank, Gaza Strip, and East Jerusalem, and recognition of Palestinian rights including the right of return. However, 
                 after only three months, the United States, Russia, the European Union (EU), and the United Nations froze financial assistance 
                 to the Hamas-led government against the wishes of the Arab League who had approved of the changes and implementations Hamas 
                 had added."),
               
               h3("Internal Palestinian Conflicts and Gaza War"),
               
               
               p("The rise of Hamas and the economic sanctions levied on the Hamas-led government led to violent internal Palestinian 
               conflicts, most notably with Fatah, the leading party in the PLO. The tension escalated into violent clashes between the 
               two factions, particularly in the Gaza Strip. These clashes were marked by a breakdown of law and order, with both sides 
               engaging in brutal tactics, including targeted killings and torture. The situation reached a climax in 2007 when Hamas forcibly 
               took control of the Gaza Strip, effectively splitting the Palestinian territories into two separate political entities, with 
               Hamas ruling Gaza and the Fatah-dominated Palestinian Authority governing the West Bank. This led to the 2008-2009 Gaza War 
               between Israel and Hamas. It began in December 2008, following the breakdown of a six-month ceasefire, during which Hamas 
               continued to fire rockets into Israeli territory. Israel launched the operation with the stated aim of stopping these rocket 
               attacks and reducing Hamas' military capability. The conflict commenced with an intensive Israeli aerial bombing campaign, 
                  followed by a ground invasion of the Gaza Strip. The war lasted for three weeks, resulting in an estimated 1,400
                  Palestinian deaths and 13 Israeli deaths. The densely populated Gaza Strip suffered extensive damage to its 
                  infrastructure, including schools, hospitals, and residential buildings. The civilian toll and humanitarian impact 
                  left by Israel drew widespread international condemnation and raised serious concerns about possible violations 
                  of international law, including accusations of war crimes."),
               
               h3("2008-2014"),
               
               p("Underlying issues remained unresolved, while sporadic rocket fire from Gaza into Israeli territory continued, met with retaliatory 
               airstrikes by Israel. In the Gaza Strip, the humanitarian situation deteriorated due to an Israeli-Egyptian blockade, imposing a land,
               sea, and air blockade. According to Israel, the blockade's goal was to restrict the weapons flow to Hamas. However, this has also severely 
               limited the movement of people and goods, impacting the daily life of Gazans. Palestinians call the blockade collective punishment 
               and say it has turned Gaza into an open-air prison. The name is attributed to Gaza as Palestinians in the enclave need approval from 
               Israel or Egypt to exit, a request frequently denied. Also, the movement of water, fuel, and other essential supplies is also constrained. 
               According to the UN, the lack of supplies led to 80% of Palestinians in Gaza living in poverty. At the same time, only 5% have access to 
               clean drinking water due to the limited supply. These restrictions placed on Gaza by Israel led to the 2014 Gaza War, triggered by the
               murder of three Israelis by Hamas and the subsequent murder of a Palestinian teenager by Israel."),
               p(" "),
                    
               p("At the same time, the Israeli government continued its policy of expanding settlements in the West Bank and East Jerusalem,
               a move that continued to be condemned by the international community as a violation of international law. The expansion of 
               settlements, along with the associated infrastructure such as roads and security barriers into the area that were promised to 
               Palestinians led to restricted movement, land confiscation, and economic hardships in the West Bank. In Jerusalem, tensions were 
               particularly high due to the city's religious and national significance. The Israeli government's policies in East Jerusalem, 
               including house demolitions, settlement expansion, and revocation of residency rights for Palestinians, were sources of deep 
               resentment and conflict. Clashes between Palestinians and Israeli security forces grew in number, often triggered by issues related 
               to the Temple Mount compound, a site sacred to Christians, Muslims, and Jews. These tensions led to various forms of resistance in 
               the West Bank by Palestinians due to the increasing number of Israeli settlers moving into the West Bank."),
               
               h3("2014 Gaza War - 2020"),
               
               p("In response to Hamas in Gaza, Israel launched a military operation to halt the rocket attacks and destroy Hamas'
                  tunnel network. The conflict involved intensive aerial bombardments by Israel with counter-rockets fired by Hamas. 
                  A ground invasion by Israeli forces into Gaza resulted in intense urban combat. The war led to a high number of
                  casualties, again predominantly among Palestinians with 2,310 deaths compared to 67 Israeli deaths. The war drew
                  widespread international attention and condemnation, with concerns raised about the proportionality of Israel's
                  military response. Several temporary ceasefires were brokered but quickly collapsed. The war eventually ended with
                  an open-ended ceasefire agreement, mediated by Egypt."),
               
               p("The aftermath of the 2014 Gaza War left deep wounds, with Gaza's humanitarian situation worsening due to the 
                  ongoing blockade by Israel and Egypt and internal Palestinian political divisions. Israeli settlement expansion 
                  in the West Bank continued, further complicating the prospects for a two-state solution and often sparking 
                  international condemnation. Periodic outbreaks of violence in Gaza and the West Bank, including an escalation 
                  in 2018, underscored the volatility of the situation. Meanwhile, Jerusalem remained a flashpoint, with tensions 
                  particularly high around the Temple Mount complex. The city witnessed several waves of violence, including 
                  stabbing attacks, shootings, and clashes between Palestinians and Israeli security forces. The situation in the 
                  West Bank was also unstable, with frequent raids by the Israeli military, clashes during protests, and sporadic 
                  acts of violence. Efforts to restart peace negotiations largely stalled, with the U.S. government's recognition of 
                  Jerusalem as Israel's capital in 2017 and the subsequent relocation of its embassy there in 2018 exacerbating 
                  tensions. The U.S.-proposed Peace to Prosperity plan in 2020, which heavily favored Israeli positions, was 
                  rejected by Palestinians and received a mixed response internationally. Political change occurred in Israel headed by 
                  long-time Prime Minister Benjamin Netanyahu and others. Palestinian leadership faced their own challenges, with 
                  President Mahmoud Abbas's extended term and the postponement of elections leading to questions about democratic 
                  legitimacy and representation."), 
               
               h3("2021 Conflict in Jerusalem and Present Day"),
               
               p("In 2021, conflict arose in May ignited in Jerusalem with an attack on Muslims in Al-Aqsa by the Israeli Defense
                  Force. Tensions had been rising in the city due to proposed evictions of Palestinian families in the Sheikh Jarrah 
                  neighborhood, which many viewed as part of a broader effort to alter the demographic balance of East Jerusalem. 
                  The situation escalated during the final days of Ramadan, when Israeli police raided the Al-Aqsa Mosque compound 
                  amid Palestinian protests, using stun grenades and rubber bullets. The raid, which resulted in numerous injuries, 
                  was widely condemned by Palestinians and the international community and was seen as a provocative act in a 
                  sensitive and sacred location. In response to these events, Hamas launched rockets into Israeli territory from Gaza, 
                  marking the beginning of an 11-day conflict. Israel responded with a series of airstrikes targeting Gaza, aiming to
                  dismantle Hamas' military infrastructure. According to Btselem, the conflict led to the death of 313 Palestinians in 
                  2021, whereas 9 Israelis lost their lives."),
               
               p("In 2023, the Israeli-Palestinian conflict has continued to be marked by a cycle of violence and escalating tensions. Incidents 
               such as a shooting attack by Palestinian gunmen at a gas station in the West Bank in June, which resulted in the gunmen being shot 
               dead, have contributed to this volatile atmosphere. In July, an Israeli assault took place on the Jenin refugee camp, leading to a 
               minimum of 12 Palestinian fatalities, which included three children. This marked the most extensive incursion in the West Bank since 
               the second Intifada. This event prompted concerns about escalating violence in the region, leading the UN Middle East envoy to express 
               deep alarm at the continued loss of civilian lives, urging all sides to refrain from actions that could inflame the situation further. 
               However, Israel has intensified raids in the West Bank, conducting searches, arrests, and home demolitions in response to a rise in 
               attacks by Palestinian militants. From the beginning of the year until October 2023, there have been 233 Palestinian fatalities in the 
               West Bank, in sharp contrast to the four Israeli deaths. Additionally, the UN Secretary-General António Guterres expressed deep concern 
               over the Israeli government's decision in June to amend its settlement planning procedures in the occupied West Bank and East Jerusalem. 
               This change is anticipated to accelerate the growth of Israeli settlements, a factor perceived as a major contributor to tensions and 
               violence in the region."),
               
               p("Then, on October 7, 2023, conflict erupted between Israel and Hamas after Hamas launched a coordinated land, sea, and air assault 
               on Israel from the Gaza Strip with the first attack located at civilians attending a concert near the Israeli-Gaza border. In 
               response, the Israel Defense Forces launched air strikes on Gaza, followed by a ground incursion with unequivocal support from 
               the U.S. The fighting has led to an estimated 1,200 deaths in Israel and the ground incursion resulting in over 1.4 million 
               Palestinians in Gaza becoming internally displaced and at least 17,177 Palestinians deaths in Gaza since October 7. There have also 
               been an added 219 Palestinian deaths in the West Bank since the October 7 attack, making this the deadliest conflict for 
               Palestinians since the 1948 Arab-Israeli war. To add to the situation, Israel has blocked humanitarian aid from reaching the 
               Gaza Strip, even temporarily cutting off the water supply to Gaza. In addition, Israel has bombed two separate hospitals and a 
               refugee camp, leading to UN condemnation. On November 24, a negotiated ceasefire started. The Hamas 
               attack on October 7 drew widespread international condemnation, with many countries denouncing the terrorism against civilians. 
               However, the deepening humanitarian crisis in Gaza has also led to significant international pressure on Israel to allow limited 
               aid into the territory."),
               
             )
    ),    
    tabPanel("Brief History of Israeli Settlements",
                fluid = TRUE,
                tags$div(
                class = "custom-text-container",
                     
                h3("Israeli Settlement Overview"),
                p("Israeli settlements in the West Bank are a highly contentious issue in the broader Israeli-Palestinian conflict.
                The settlements, which are considered illegal under international law due to their location on territory meant to be
                governed by the Palestinian Authority, have been a source of tension between Israelis and Palestinians for decades.
                Meanwhile, Israel claims they are necessary for defense and security against attacks on their citizens. Settlement construction
                began in 1967 and continues to today. At points, the United States, Israel's biggest ally, has asked for construction of
                settlements to halt but they have mostly agreed with Israel on needing them for security. These settlements are also
                important for Israel as they can regain control over areas in the West Bank and limit the Palestinian territory, economy,
                and strength. In turn, Palestinians are angered but unable to respond due to their limited rights and lack of power. 
                The most disputed area for settlements is in East Jerusalem, primarily because of the city's religious significance."),
                     
                h3("Early Settlements"),
                     
                p("After Israel took back Jerusalem in the Six-Day War, Israel decided to take more land for the new country. This was achieved 
                by constructing Israeli settlements or villages in the remaining Palestinian territories, namely Gaza and the West Bank. Starting
                in 1967, Israel built in the West Bank through a security doctrine, building in the Jordan Valley, an area well known for its
                agriculture, before de facto annexing East Jerusalem in 1968. Most settlements started as military outposts before transitioning
                into civilian areas. The settlements, even in their early stages, were condemned with international calls, such as Resolution 298
                in 1971 by the United Nations Security Council (UNSC), criticizing the transfer of populations into the occupied section of
                Jerusalem. However, the construction of settlements ramped up in 1977 due to two new plans: The Sharon Plan and the Drobles Plan."),
                     
                h3("A New Agenda"),
                     
                p("The Sharon Plan had four parts. The first was the establishment of urban settlements on the western reaches of the 
                 Samaria Mountains. Second was extending Jewish settlements in the Jordan Valley. Next was encircling East Jerusalem with
                 a “belt” of Jewish settlements. Lastly was to build roads linking the Western and Eastern Zones, along with settlements to 
                 help secure them. The four parts followed four objectives of the Sharon Plan: prevent the Palestinian civilian population
                 from entering Israel, create a buffer between West Bank Palestinians and Arab-populated Israeli territories, control the 
                 high ground overlooking the Israeli coastal plain, and ensure the security of Lydda International Airport. Meanwhile, the 
                 Drobles Plan called for groups of settlements throughout the West Bank with the explicit goal of disrupting the Palestinians’ 
                 demographic continuity, entrenching Israel’s presence throughout the territory. These two plans combined led to rapid growth
                 in the construction of settlements from 1977 to 1984."),
                     
                h3("Current Day"),
                     
                p("Although the construction of settlements has decreased from its peak, there has been an increase in the number of Israelis relocating
               to these settlements.  While the UN and the international community have condemned the settlements, they have continued to increase and 
               grow in size due to the support of the U.S. For example, the International Court of Justice (ICJ) declared the settlements to be illegal
               in a 2004 advisory opinion. While the U.S. does not agree with the settlements, they will not agree with retribution against Israel. 
               Meanwhile, the international community views the settlements as a ruthless policy of land confiscation, illegal settlement and 
               dispossession, coupled with rampant discrimination, inflicting immense suffering on Palestinians while depriving them of their 
               basic rights. Since the settlements have fractured the West Bank, making it difficult to travel between Palestinian cities and 
               towns due to movement restrictions, Israel can restrict whether Palestinians can travel, go abroad, visit their relatives, earn 
               a living, attend a protest, access their farmland, or access electricity or a clean water supply. 
                 
                 
               Presently, settlements resemble residential communities, exhibiting a range in size and population, from cities and towns to smaller 
               communities with only a few dozen residents. Many settlements have become like suburban towns, with residential neighborhoods, 
               shopping centers, industrial areas, and schools. However, they are built on Palestinian land with the existing Palestinian 
               structures and homes often demolished or confiscated by the Israeli government. Even with the international outcry, Israel 
               continues to build settlements in the West Bank to this day. In June of 2023, Israel announced plans to build 5,700 new homes 
               in the West Bank to increase the already 700,000 Israeli population in the West Bank. These settlements will continue to play 
               a role in the ongoing Israel-Palestine conflict."),
                   )
    ),
    tabPanel("Viewshed Instructions",
             fluid = TRUE,
             tags$div(
               class = "custom-text-container",
               
               h3("Loading the Data"),
               p("To load the data, make sure you have downloaded the file '2m_Res250_25%_2023Nov_23_RC_vsf.Rdata' and placed it into the 
                 'LOS Data' folder from GitHub. On the Load Data tab, select your desired file (for this project, use the aforementioned file) and 
                 press 'Load File'. From here, the list of Israeli settlements in the West Bank appears. To see the location of an individual
                 settlement, check the box next to the name."),
               
               h3("Running the Cumulative Viewshed"),
               p("After loading the data, the list of settlements should populate into the 'Cumulative Viewshed' tab. Here, the intended viewing is
                 by using the 'Select All' feature, however, you can choose to view individual settlements if desired. After selecting all or
                 choosing a settlement, press the 'Calculate' button to run the specific viewshed map. When viewing the intended way,
                 it may take between 1-3 minutes to load depending on your computer's processing power."),
             )
    ),
    tabPanel("Load Data", fluid = TRUE,
             # titlePanel("Viewshed Analysis"),
             sidebarLayout(
               sidebarPanel(
                 p(),
                 textInput(inputId="mapheight_M", label="Map vertical size on screen",value = "700px"),
                 selectInput(inputId="Filename_M",label ="Filename",choices = LOSfilesnames,selected = LOSfilesnames[1] ),
                 actionButton("Load_M",label =  "Load File"),
                 uiOutput("ANTControls1_M")
               ), 
               mainPanel(
                 uiOutput("LeafletMAP_M"),
                 h4("INPUT FILE:",align = "center"),
                 h4(textOutput("txtOutput2_M")),
                 textOutput ("txtOutput_coords_M")
               )
             )
    ),
    tabPanel("Cumulative Viewshed", fluid = TRUE,
             # titlePanel("Viewshed Analysis"),
             sidebarLayout(
               sidebarPanel(
                 p(),
                 h4("INPUT FILE:",align = "center"),
                 strong(h4(textOutput("txtOutput2_C"),align = "center")),
                 textInput(inputId="mapheight_C", label="Map vertical size  on screen",value = "700px"),
                 actionButton("recalc_C",label =  "Calculate"),
                 actionButton("selectall","Select All"),
                 actionButton("unselectall", "Unselect All"),
                 uiOutput("ANTControls1_C"),
                 strong(h4(textOutput("savedcumul"),align = "center")),
               ),
               mainPanel(
                 uiOutput("LeafletMAP_C"),
                 h4("The Choosen Settlements are:",align = "center"),
                 textOutput("txtOutput1_C"),
                 p(),
               )
              )
     ),
    tabPanel("Viewshed Analysis",
             fluid = TRUE,
             tags$div(
               class = "custom-text-container",
               
               h3("Percent Seen by Settlements"),
               
               uiOutput("combinedCoverageText"),
               
               h3("Major Cities"),
               
               p("The encompassing area marked in purple, when including all settlements, covers almost all of the major Palestinian cities.
               This could imply significant visibility and oversight capabilities from the settlements. In the map analysis, all major
               Palestinian cities in the West Bank with populations exceeding 50,000 residents — specifically Hebron (with settlements in
               its eastern part), East Jerusalem, Nablus (which is encircled by settlements), Ramallah, al-Bireh, Yatta, and Qalqilya — are
               covered in purple. However, Jenin, often regarded as the center of Palestinian resistance and militancy, is not extensively
               covered in purple on the map, with its main downtown area appearing in tan. This lack of coverage is the same with Tulkarm,
               although since this city is on the border of the West Bank and Israel, it is already policed and monitored by Israel. The tan
               area located in the southern mid-west of the map is the major city of Bet Shemesh. This city is not within the West Bank but 
               is part of Israel. This region appears tan due to the specific focus of the map on the West Bank and the exclusion  of Israeli
               cities in the database used for the analysis, resulting in Bet Shemesh's tan representation on the map."), 
               
               h3("Populated Tan Areas"),
                 
               p("Upon a detailed examination of the map, the tan areas, which represent regions not visible from Israeli settlements, are 
               predominantly desolate mountain ranges or farmland. Therefore, these areas are not under surveillance from the settlements. 
               However, two populated areas appear tan on the viewshed. The first, as previously mentioned, is Jenin while the second is the
               collection of four towns in a line located in the Marj Sanur Valley: Sanur, Meithalun, Al-Judeida, and Siris. Located about 
               halfway between Jenin and Nablus in the northern center of the West Bank, these cities are home to 25,000 Palestinians 
               combined. The lack of Israeli settlements in the Marj Sanur valley is attributed to geographical, historical, and political 
               factors. Marj Sanur, as a fertile valley and a seasonal lake located within the closed basin of the northern mountains of the 
               West Bank, indeed represents agriculturally valuable land. This characteristic would typically make such an area a viable and 
               attractive location for settlement due to its potential for agricultural productivity. However, the establishment of settlements 
               is influenced by more than agricultural value. In 2005, as part of the Israeli disengagement from Gaza, four Israeli settlements 
               were evacuated from the Marj Sanur valley, the only four closed settlements as part of the deal outside of Gaza. Due to this
               deal, these settlements in this area have never returned, leaving it as one of the two areas, along with Jenin, outside of the
               watch of Israeli settlements."),
               
               h3("Southeast West Bank"),
               
               p("The southeastern area of the West Bank being mostly tan on the map, indicating limited visibility from Israeli settlements, 
               aligns with the region's geographical and demographic characteristics. This area is predominantly mountainous and largely 
               desolate, with sparse population density. The lack of significant human settlement or strategic interest diminishes the 
               incentive for establishing visibility or surveillance from Israeli settlements. The consistent visibility from Israeli 
               settlements across the West Bank-Jordan border reflects a strategic approach by the Israeli government. This pattern of settlement,
               particularly in the Jordan Valley along the border, aligns with Israel's broader security objectives. The settlements are
               strategically positioned to function as observation points and buffers, enhancing Israel's ability to monitor and potentially 
               control border activities. Historically, the Jordan Valley's critical role in Israel's security strategy is well-documented, 
               influencing settlement patterns and reinforcing the significance of this region in national defense policy."),
               
               h3("Conclusion"),
               
               uiOutput("combinedCoverageText2"),
               
               p("The lack of visibility in Jenin, a city noted for its historical resistance, and the agriculturally rich yet politically 
               complex Marj Sanur Valley, reflects the varied considerations that influence settlement placement. Furthermore, the 
               southeastern West Bank, marked mostly in tan due to its mountainous and sparsely populated terrain, aligns with the strategic
               pattern of settlement visibility."),
               
               p("Moreover, the continuous line of settlements along the West Bank-Jordan border underlines a deliberate strategy, likely 
               aimed at monitoring and controlling this critical boundary. The Jordan Valley's pivotal role in Israel's security strategy is 
               well-documented, underscoring the strategic importance of this region. This analysis not only demonstrates the extensive reach 
               of settlement visibility across the West Bank but also highlights the complex interplay of geographical, historical, and 
               political factors shaping the Israeli-Palestinian landscape."),
             )
    ),
    tabPanel("Works Cited",
             fluid = TRUE,
             div(
               class = "custom-text-container",
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Load data
  output$home_img <- renderImage({
    
    list(src = "www/444411.png",
         width = "100%",
         height = 600)
    
  }, deleteFile = F)
  
  output$combinedCoverageText <- renderUI({
    coverage_value <- combined_coverage() * 100
    
    p(paste("The Historical Viewshed tab presents an overview of the areas in the West Bank visible from the perimeters of 
    Israeli settlements. The purple regions on the map indicate highly visible areas from various vantage points or 
    settlements, while the green dots represent the specific settlements currently selected in the analysis. The orange or 
    yellow regions depicted on the map can be seen from the settlements, indicating a moderate level of visibility, however, 
    they do not have as extensive coverage as the purple areas. The tan areas, or those left blank on the map, represent 
    regions not visible from the borders of a settlement. Running an analysis on the map and finding", round(coverage_value, 2), 
    "% of the area is visible in purple from the combined vantage points of all settlements suggests a strategic placement atop 
    elevations, possibly indicating that surveillance could be among the purposes of these settlement locations. This high 
    visibility coverage aligns with the notion that settlements situated on higher ground have a broader observational reach 
    over the surrounding territory."))
  })
  
  output$combinedCoverageText2 <- renderUI({
    coverage_value <- combined_coverage() * 100
    
    p(paste("In conclusion, the Viewshed analysis of the West Bank, reveals significant visibility from Israeli settlements 
    across the region. A substantial", round(coverage_value, 2), "% of the area, marked in purple, is visible from these 
    settlements, indicating a strategic placement for surveillance purposes. This is particularly evident in the coverage of 
    almost all major Palestinian cities, suggesting extensive oversight capabilities. However, certain areas in Jenin and part 
    of the Marj Sanur Valley, remain outside of this surveillance scope."))
  })
  
  observeEvent(input$Load_M, {
    input$Load_M
    isolate({
      load(paste0("LOSData/",input$Filename_M,"_vsf.Rdata"))
      ANTS.df <<- ANTS.df 
      LOSLayers <<- LOSLayers
      DEM <<- DEM
      
      if ('999' %in% ANTS.df$ID) {
        InvalidLayerFlag <- TRUE
        IvalidLayerIdx <<- which(ANTS.df$ID == '999')
        ANTS.df <<- ANTS.df[-IvalidLayerIdx,]
      } else {
        InvLayer <- LOSLayers[[1]]
        invIdx <- which(is.na(getValues(InvLayer)))
        InvLayer[invIdx] <- TRUE
        vIdx <- which(!is.na(getValues(InvLayer)))
        InvLayer[vIdx] <- FALSE
        InvLayer[1] <- TRUE
        LOSLayers <<- addLayer(LOSLayers, InvLayer)
        IvalidLayerIdx <<- nrow(ANTS.df) + 1
      }
      
      coordinates(ANTS.df) <- ~LON+LAT
      
      # Unique settlement names
      unique_settlements <- sort(unique(ANTS.df$ANTName))
      output$ANTControls1_M <- renderUI({
        checkboxGroupInput("antID_M", "Settlements", 
                           choiceValues = unique_settlements, 
                           choiceNames = unique_settlements, 
                           inline = FALSE)
      })
      
      
      ANT_indeces_M <- reactive({
        selected_settlements <- input$antID_M
        which(ANTS.df$ANTName %in% selected_settlements)
      })
      
      pal <- colorNumeric(palette = "Spectral", domain = seq(min(DEM@data@min), max(DEM@data@max), length.out = 50), reverse = T)
      output$mymap_M <- renderLeaflet({
        leaflet() %>%
          addProviderTiles('Esri.WorldImagery') %>%
          addRasterImage(DEM, color = pal, opacity = 0.8) %>%
          addRasterImage(LOSLayers[[IvalidLayerIdx]], colors = cpal_inv, opacity = 0.8) %>%
          addLegend(pal = pal, values = values(DEM), title = "Elevation") %>%
          addCircles(data = ANTS.df, weight = 5, fillOpacity = 1,
                     color = ~ifelse(ANTS.df$ANTName %in% input$antID_M, "red", "blue"),
                     popup = ~htmlEscape(paste0("ID=", as.character(ANTS.df$ID),
                                                ",", ANTS.df$ANTName,
                                                ", Ground elevation=", ANTS.df$GroundASL,
                                                "m, Settlement height=", ANTS.df$Towerheight,
                                                "m, (", ANTS.df$LAT, ",", ANTS.df$LON, ")"))) %>%
          addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 250, metric = TRUE, imperial = T))
      })
    })}) 
  
  output$LeafletMAP_M <- renderUI({leafletOutput("mymap_M",height =input$mapheight_M )})
  output$txtOutput2_M <- renderText({      paste0( input$Filename_M)    })
  ANT_indeces_M       <- reactive({which(ANTS.df$ID %in% input$antID_M)}) 
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    output$txtOutput_coords_M <- renderText({      paste("lat=",feature$geometry$coordinates[[2]],"lon=",feature$geometry$coordinates[[1]])    })
  })
  
  # Cumulative Viewshed tab
  observeEvent(input$Load_M, {
    input$Load_M
    isolate({
      # Create a checkbox group input with unique settlement names
      unique_settlements <- sort(unique(ANTS.df$ANTName))
      output$ANTControls1_C <- renderUI({
        checkboxGroupInput("antSettlement_C", "Choose Settlements. For optimal viewing, select all settlements. However, please be patient as it can take up to a few minutes to load depending on the number of settlements included.", choices = unique_settlements)
      })
  
      values <- reactiveValues(unique_settlements = unique_settlements)})
  })
  
  # Reactive expression for selected settlements
  ANT_indeces_C <- reactive({
    selected_settlements <- input$antSettlement_C
    which(ANTS.df$ANTName %in% selected_settlements)
  })
  
  combined_coverage <- reactive({
    .7079871
  })
  
  observeEvent(input$selectall, {
    # Retrieve the unique settlement names
    unique_settlements <- unique(ANTS.df$ANTName)
    
    # Update the checkbox group to select all settlements
    updateCheckboxGroupInput(session, "antSettlement_C", selected = unique_settlements)
  })
  
  observeEvent(input$unselectall, {
    updateCheckboxGroupInput(session, "antSettlement_C", selected = character(0))
  })
  
  
  output$LeafletMAP_C   <- renderUI({leafletOutput("mymap_C",height =input$mapheight_C )})
  output$txtOutput1_C   <-  renderText({  paste0(ANTS.df$ANTName[which(ANTS.df$ID %in% input$antID_C)],",") })
  
  observeEvent(input$Load_M, {
    input$Load_M
    isolate({output$txtOutput2_C   <-  renderText({      paste0( input$Filename_M)    })})})
  
  # Render leaflet map on 'Recalc'
  observeEvent(input$recalc_C, {
    output$mymap_C <- renderLeaflet({
      input$recalc_C
      isolate({
        sumLayers <- sum(LOSLayers[[ANT_indeces_C()]])
        maxsumLayers <- cellStats(sum(LOSLayers[[ANT_indeces_C()]]), "max")
        cpal_C <- c(grey.colors(2, start = 0.8, end = 1), rainbow(4, start = 0, end = 0.2, rev = T), rep("#B800FF", max(0, maxsumLayers - 6)))
        val_C = as.numeric(1:length(cpal_C))
        pal_C = colorNumeric(cpal_C, val_C, na.color = "transparent")
        leaflet() %>%
          addProviderTiles('Esri.WorldImagery') %>%
          addRasterImage(sumLayers, colors = pal_C(0:maxsumLayers), opacity = 0.5)%>%
          addRasterImage(LOSLayers[[IvalidLayerIdx]], colors = cpal_inv, opacity = 0.8) %>%
          addLegend(colors = pal_C(0:maxsumLayers),labels =(0:maxsumLayers), title = "Number of Settlements")%>%
          addMarkers(data=ANTS.df[ANT_indeces_C(),], icon = ~ IconsVis[[1]],
                     popup = ~htmlEscape(paste0("ID=",as.character(ANTS.df$ID[ANT_indeces_C()]),",",ANTS.df$ANTName[ANT_indeces_C()])))%>%
          addScaleBar( position =  "bottomleft", options = scaleBarOptions(maxWidth = 250, metric = TRUE,imperial = T))
      })
    })
  })
  
}

coverage("viewshed_map.png")

runApp(shinyApp(ui, server),launch.browser = TRUE)
