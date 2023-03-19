#01 MARKET BASKET ANALYSIS
# Problem Statement
# Consider yourself to be the newly appointed manager of a retail-store 'ALL-MART'. 
#Your first task as manager of the store is to increase cross-selling

#Tasks to be Performed
#1 Understand the transactions
#     A)Find the total number of transactions
#     B) Find the total number of items in the inventory
#     C) Find the total number of items purchased.
#     D) Find out the 10 most frequently bought items & make a plot.

#2 Building 1st set of association rules
#     A) Build apriori algorithm with support value->  0.005 & Confidence value->   0.8
#     B) Sort the rules w.r.t confidence & inspect the top 5 rules & the bottom 5 rules
#     C) Sort the rules w.r.t lift & Inspect the top 5 rules
#     D) Plot the rules using different methods.

#3 Building 2nd set of association rules
#     A) Build apriori algorithm with support value->  0.009 & Confidence value->   0.3
#     B) Sort the rules w.r.t confidence & inspect the top 5 rules & the bottom 5 rules
#     C) Plot the rules using different methods.

#4 Building 3 rd set of association rules
#     A) Build apriori algorithm with support value->  0.02 & Confidence value->   0.5
#     B) Sort the rules w.r.t confidence & inspect the top 5 rules & the bottom 5 rules
#     C) Plot the rules using different methods.

#====================================================================================================================

library(arules)
library(arulesViz)
# file location is in the local drive
market_basket<-read.transactions(
		file="D:\\1INTELLIPAAT SQL\\RSELF STUDY\\PROJECTS DEADLINE 5JUN22\\PROJECT ATTACHMENT DOWNLOADS\\PROJECT1- MARKET BASKET ANALYSIS\\market_basket.csv",
	sep=',',
	quote="",
	format = 'basket',
	rm.duplicates = TRUE,
	skip=1
)

#distribution of transactions with duplicates:
# items
# 1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   20   22   23   25 
# 1029  473  266  159   83   61   52   32   16   15   10   11    4    3    2    1    5    1    1    1    2    1 
# 27   34   52 
# 1    1    1 


summary(market_basket)

# transactions as itemMatrix in sparse format with
# 18440 rows (elements/itemsets/transactions) and
# 22346 columns (items) and a density of 0.0009915565 
# 
# most frequent items:
# 	WHITE HANGING HEART T-LIGHT HOLDER           REGENCY CAKESTAND 3 TIER            JUMBO BAG RED RETROSPOT 
# 1971                               1703                               1598 
# PARTY BUNTING      ASSORTED COLOUR BIRD ORNAMENT                            (Other) 
# 1379                               1375                             400555 
# 
# element (itemset/transaction) length distribution:
# 	sizes
# 2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23 
# 1359  715  602  616  660  588  568  584  608  508  542  499  475  509  543  544  466  433  477  420  409  341 

# there are 1359 transactions in which no if items purchased was 2
# there are 327 transactions in which no if items purchased was 24 and so on...

# 24   25   26   27   28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45 
# 327  311  239  266  250  216  264  232  196  170  165  170  144  124  128  110  123  121  111  106   94   92 

# 46   47   48   49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64   65   66   67 
# 83   89   82   79   73   79   62   55   64   67   62   48   53   46   47   37   45   35   31   33   32   39 

# 68   69   70   71   72   73   74   75   76   77   78   79   80   81   82   83   84   85   86   87   88   89 
# 38   34   23   30   30   14   25   32   24   20   18   16    9   16   18   19   16   18   15   11   14   13 

# 90   91   92   93   94   95   96   97   98   99  100  101  102  103  104  105  106  107  108  109  110  111 
# 9    8   11   15   12    9    5    8   10   10    3    7   11    3    9    7    2    3    3    6    4    3 

# 112  113  114  115  116  117  118  119  120  121  122  123  124  125  126  127  128  129  131  133  134  135 
# 3    2    5    4    4    8    5    5    5    5    4    8    5    1    4    5    3    4    2    1    4    1 

# 136  137  138  139  140  141  142  143  144  145  147  149  150  151  152  153  156  158  159  160  165  166 
# 1    3    2    2    1    2    2    2    2    1    5    1    1    1    2    1    1    1    1    1    1    1 

# 167  168  170  171  172  178  179  181  182  186  188  194  195  197  205  206  209  212  221  231  251  260 
# 1    1    1    1    1    2    2    1    2    1    1    1    1    1    1    1    1    1    1    1    1    1 

# 264  274  284  334  340  352  357  367  380  423  441  443  530  534  548 
# 1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 

# there was 1 transaction in which no if items purchased was 548


# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00    8.00   16.00   22.16   28.00  548.00 

#summary function output

# includes extended item information - examples:
# 	labels
# 1      "ASSORTED FLOWER COLOUR ""LEIS"""
# 2  "CHARLIE+LOLA""EXTREMELY BUSY"" SIGN"
# 3 "FLOWER GLASS GARLAND NECKL.36""BLACK"


#Compute the number of items that were purchased : 
18440*22346*0.0009915565
#[1] 408581

#     D) Find out the 10 most frequently bought items & make a plot.
library(dplyr)
library(arules)
library(magrittr)
market_basket %>% head(n = 5) %>% 

# [1]  {1,                                   
# 	MEDIUM CERAMIC TOP STORAGE JAR}      
# [2]  {2,                                   
# 	3D DOG PICTURE PLAYING CARDS,        
# 	AIRLINE BAG VINTAGE JET SET BROWN,   
# 	ALARM CLOCK BAKELIKE CHOCOLATE,      
# 	ALARM CLOCK BAKELIKE GREEN,          
# 	ALARM CLOCK BAKELIKE ORANGE,         
# 	ALARM CLOCK BAKELIKE PINK,           
# 	ALARM CLOCK BAKELIKE RED,            
# 	BATHROOM METAL SIGN,                 
# 	BLACK CANDELABRA T-LIGHT HOLDER,     
# 	BLACK EAR MUFF HEADPHONES,           
# 	BLACK GRAND BAROQUE PHOTO FRAME,     
# 	BLUE 3 PIECE POLKADOT CUTLERY SET,   
# 	BLUE DRAWER KNOB ACRYLIC EDWARDIAN,  
# 	BOOM BOX SPEAKER BOYS,               
# 	BOX OF 6 ASSORTED COLOUR TEASPOONS,  
# 	CAMOUFLAGE EAR MUFF HEADPHONES,      
# 	CLEAR DRAWER KNOB ACRYLIC EDWARDIAN, 
# 	COLOUR GLASS. STAR T-LIGHT HOLDER,   
# 	EMERGENCY FIRST AID TIN,             
# 	FOUR HOOK  WHITE LOVEBIRDS,          
# 	GREEN DRAWER KNOB ACRYLIC EDWARDIAN, 
# 	LARGE HEART MEASURING SPOONS,        
# 	MINI PAINT SET VINTAGE,              
# 	PINK 3 PIECE POLKADOT CUTLERY SET,   
# 	PINK DRAWER KNOB ACRYLIC EDWARDIAN,  
# 	PURPLE DRAWERKNOB ACRYLIC EDWARDIAN, 
# 	RED 3 PIECE RETROSPOT CUTLERY SET,   
# 	RED DRAWER KNOB ACRYLIC EDWARDIAN,   
# 	RED TOADSTOOL LED NIGHT LIGHT,       
# 	SET OF 2 TINS VINTAGE BATHROOM,      
# 	SET/3 DECOUPAGE STACKING TINS}       
# [3]  {3,                                   
# 	3D DOG PICTURE PLAYING CARDS,        
# 	60 TEATIME FAIRY CAKE CASES,         
# 	72 SWEETHEART FAIRY CAKE CASES,      
# 	AIRLINE BAG VINTAGE JET SET BROWN,   
# 	AIRLINE BAG VINTAGE JET SET WHITE,   
# 	ALARM CLOCK BAKELIKE CHOCOLATE,      
# 	ALARM CLOCK BAKELIKE GREEN,          
# 	ALARM CLOCK BAKELIKE ORANGE,         
# 	ALARM CLOCK BAKELIKE PINK,           
# 	ALARM CLOCK BAKELIKE RED,            
# 	BLACK CANDELABRA T-LIGHT HOLDER,     
# 	BLUE NEW BAROQUE CANDLESTICK CANDLE, 
# 	BOX OF 6 ASSORTED COLOUR TEASPOONS,  
# 	CHOCOLATE CALCULATOR,                
# 	MINI LADLE LOVE HEART RED,           
# 	PACK OF 60 MUSHROOM CAKE CASES,      
# 	PACK OF 60 SPACEBOY CAKE CASES,      
# 	PINK NEW BAROQUECANDLESTICK CANDLE,  
# 	RED RETROSPOT OVEN GLOVE,            
# 	RED RETROSPOT OVEN GLOVE DOUBLE,     
# 	RED TOADSTOOL LED NIGHT LIGHT,       
# 	REGENCY CAKESTAND 3 TIER,            
# 	SANDWICH BATH SPONGE,                
# 	SET OF 2 TINS VINTAGE BATHROOM,      
# 	SET/2 RED RETROSPOT TEA TOWELS,      
# 	SMALL HEART MEASURING SPOONS,        
# 	TEA TIME OVEN GLOVE,                 
# 	TOOTHPASTE TUBE PEN,                 
# 	WOODLAND CHARLOTTE BAG}              
# [4]  {3D SHEET OF CAT STICKERS,            
# 	3D SHEET OF DOG STICKERS,            
# 	4,                                   
# 	AIRLINE BAG VINTAGE JET SET BROWN,   
# 	AIRLINE BAG VINTAGE JET SET RED,     
# 	AIRLINE BAG VINTAGE JET SET WHITE,   
# 	AIRLINE BAG VINTAGE TOKYO 78,        
# 	GIFT BAG PSYCHEDELIC APPLES,         
# 	HOLIDAY FUN LUDO,                    
# 	ICE CREAM SUNDAE LIP GLOSS,          
# 	LARGE HEART MEASURING SPOONS,        
# 	MINI PAINT SET VINTAGE,              
# 	PACK OF 60 DINOSAUR CAKE CASES,      
# 	RED DRAWER KNOB ACRYLIC EDWARDIAN,   
# 	RED RETROSPOT OVEN GLOVE DOUBLE,     
# 	RED RETROSPOT PURSE,                 
# 	RED TOADSTOOL LED NIGHT LIGHT,       
# 	REGENCY CAKESTAND 3 TIER,            
# 	ROSES REGENCY TEACUP AND SAUCER,     
# 	SET OF 2 TINS VINTAGE BATHROOM,      
# 	SMALL FOLDING SCISSOR(POINTED EDGE), 
# 	SMALL HEART MEASURING SPOONS,        
# 	TREASURE ISLAND BOOK BOX,            
# 	VINTAGE HEADS AND TAILS CARD GAME,   
# 	WATERING CAN PINK BUNNY}             
# [5]  {3D DOG PICTURE PLAYING CARDS,        
# 	5,                                   
# 	AIRLINE BAG VINTAGE JET SET BROWN,   
# 	AIRLINE BAG VINTAGE TOKYO 78,        
# 	ALARM CLOCK BAKELIKE CHOCOLATE,      
# 	ALARM CLOCK BAKELIKE RED,            
# 	COAL BLACK,                          
# 	FEATHER PEN,                         
# 	NAMASTE SWAGAT INCENSE,              
# 	RABBIT NIGHT LIGHT,                  
# 	REGENCY MILK JUG PINK,               
# 	REGENCY SUGAR BOWL GREEN,            
# 	REGENCY TEA PLATE GREEN,             
# 	REGENCY TEA PLATE PINK,              
# 	REGENCY TEA PLATE ROSES,             
# 	REGENCY TEA STRAINER,                
# 	REGENCY TEAPOT ROSES,                
# 	SMALL HEART MEASURING SPOONS,        
# 	TRIPLE HOOK ANTIQUE IVORY ROSE,      
# 	VICTORIAN SEWING KIT}                

library(dplyr)
library(RColorBrewer)

itemFrequencyPlot(x = market_basket,
									topN  = 10,
									# support =
									type  = 'absolute',
									horiz = TRUE,
									col=brewer.pal(10,'Spectral')
)

#------------------------
rule1 <- market_basket %>%
	apriori(parameter = list(supp = 0.005, conf = 0.8)) %>%
	sort(by = 'confidence')


summary(rule1)

rule1 %>% head(n = 5) %>% inspect

rule1 %>% tail(n = 5) %>% inspect

rule1 <- rule1 %>% sort(by = "lift") 

rule1 %>%  head(n = 5) %>% inspect


#plotting

plot(rule1,engine = "htmlwidget")
plot(rule1, method = "two-key",engine = "htmlwidget")
plot(rule1, method = "graph",engine = "htmlwidget")
#--------------------------------

rule2 <- market_basket %>%
	apriori(parameter = list(supp = 0.009, conf = 0.3)) %>%
	sort(by = 'confidence')


summary(rule2)

rule2 %>% head(n = 5) %>% inspect

rule2 %>% tail(n = 5) %>% inspect

#plotting
plot(rule2,engine = "htmlwidget")
plot(rule2, method = "two-key",engine = "htmlwidget")
plot(rule2, method = "graph",engine = "htmlwidget")

#--------------------------------

rule3 <- market_basket %>%
	apriori(parameter = list(supp = 0.02, conf = 0.5)) %>%
	sort(by = 'support')


summary(rule3)

rule3 %>% head(n = 5) %>% inspect

rule3 %>% tail(n = 5) %>% inspect

#plotting
plot(rule3,engine = "htmlwidget")
plot(rule3, method = "two-key",engine = "htmlwidget")
plot(rule3, method = "graph",engine = "htmlwidget")


