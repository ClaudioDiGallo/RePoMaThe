
# KnownBots ---------------------------------------------------------------
library(stringr)
library(tidyverse)
source(paste0(masterarbeit_code_filepath,"Dependencies.R"))
source(paste0(masterarbeit_code_filepath,"functions.R"))


#The following List is from https://www.reddit.com/r/autowikibot/wiki/redditbots
#This is of course by no means exhaustive. But all of those are known and benevolent Bots




#Composing the Corpus
bots_corpus <- "/u/A858DE45F56D9BC9 	Unknown 	Unknown 	Active - Nil - 3 hrs
/u/AAbot 	submission in /r/asianamerican 	info about artist 	Inactive - 0 mins - 5 mins
/u/ADHDbot 	- 	moderator @ /r/ADHD 	Active - 30 hrs - 1198 hrs
/u/ALTcointip 	+/u/altcointip <currency numbers> 	tip bot 	Active - 1 hrs - 6 mins
/u/AVR_Modbot 	- 	moderator @ /r/aussievapers 	Dormant - 2 hrs - Nil
/u/A_random_gif 	random gif 	posts random gif from /r/gifs 	Dormant - 53 hrs - Nil
/u/AltCodeBot 	Alt-code characters in comment 	Keyboard shortcuts for them 	Dormant - 28 mins - Nil
/u/Antiracism_Bot 	racial slur in comment 	counts and mentions 	Dormant - 58 mins - Nil
/u/ApiContraption 	submission in /r/photoshopbattles 	collects non-photoshop comments 	Active - 18 mins - Nil
/u/AssHatBot 	- 	moderator @ /r/AssHatHackers 	Active - 79 hrs - Nil
/u/AtheismModBot 	- 	moderator @ multiple 	Active - 2 hrs - 2 hrs
/u/AutoInsult 	auto insult: <target> 	generates insult 	Dormant - 2 hrs - 2 hrs
/u/BELITipBot 	+/u/belitipbot <value> belicoins 	tip bot for BeliCoin (see /r/BeliCoin) 	Active - 23 mins - Nil
/u/BadLinguisticsBot 	submission in /r/badlinguistics 	rehost bot @ /r/badlinguistics 	Active - 11 hrs - Nil
/u/BanishedBot 	- 	moderator @ /r/Banished 	Active - 52 mins - Nil
/u/BeetusBot 	submission in /r/fatpeoplestories 	posts OPs other submissions 	Active - 1 hrs - Nil
/u/BensonTheBot 	- 	moderator @ /r/explainlikeIAmA 	Active - 100 hrs - Nil
/u/Bible_Verses_Bot 	- 	- 	deleted
/u/BlackjackBot 	submission in /r/RoboCasino 	Casino AI 	Dormant - 1 hrs - Nil
/u/BlockchainBot 	** 		Active - Nil - Nil
/u/Brigade_Bot 	Non- no-participation links from /r/conspiratard 	Warns and gives No-participation link 	Active - 36 hrs - Nil
/u/Bronze-Bot 	- 	- 	deleted
/u/CAH_BLACK_BOT 	submission in /r/cahideas 	Collects/processes comments from submission 	Active - 4 hrs - Nil
/u/CHART_BOT 	submission in /r/CHART_BOT 	Charts OPs user statistics 	Active - 2 hrs - Nil
/u/CLOSING_PARENTHESIS 	** 		Active - 13 mins - Nil
/u/CPTModBot 	- 	moderator @ /r/CasualPokemonTrades 	Active - 1 hrs - Nil
/u/Cakeday-Bot 	- 	- 	deleted
/u/CalvinBot 	(scheduled) 	Daily post in /r/calvinandhobbes 	Active - 73 hrs - 22 hrs
/u/CaptionBot 	submission is /r/AdviceAnimals 	Posts meme captions in comments 	Active - 10 mins - 317 hrs
/u/CarterDugSubLinkBot 	Keyword subreddit in submission title in specific subs 	posts /r/(subreddit) in comments 	Active - 76 hrs - Nil
/u/CasualMetricBot 	+/u/CasualMetricBot 	Converts non-metric units in metric units from call comment and its parent 	Active - 8 hrs - Nil
/u/Chemistry_Bot 	/u/Chemistry_Bot 	Extract chemical compund names from call comment and replies with info on it. 	Dormant - 13 hrs - Nil
/u/ChristianityBot 	- 	moderator @ /r/Christianity 	Active - 19 hrs - 1310 hrs
/u/Codebreakerbreaker 	comment by /u/Comment_Codebreaker 	Post bold characters from that comment 	Dormant - 3 hrs - Nil
/u/Comment_Codebreaker 	Unknown 	Creates sentences from random sequential characters in chosen comment 	Dormant - 3 hrs - Nil
/u/ComplimentingBot 	- 	- 	deleted
/u/CreepierSmileBot 	comment by /u/CreepierSmileBot 	posts (??° ???? ??°) as reply 	Dormant - 40 mins - Nil
/u/CreepySmileBot 	???_??? in comment body 	posts ????????? as reply 	Active - 19 mins - Nil
/u/CuteBot6969 	cute, adorable etc. in comment body 	Replies with (????????????)???*:?????????°.·° <sentence transcribed to accented characters> °??????=;, ??? 	Active - 46 mins - Nil
/u/DDBotIndia 	Scheduled 	Daily Discussion poster @ /r/india 	Active - Nil - Nil
/u/DNotesTip 	+/u/dnotestip <value> notes 	tip bot for DNotesTip (see /r/dnotes) 	Active - 0 mins - Nil
/u/DRKTipBot 	+tip DRK <value> or +tip D <value> 	tipbot for Darkcoin (see /r/DRKCoin) 	Active - 2 hrs - Nil
/u/DefinitelyBot 	wrongly spelt definitely in self-post text. 	replies with I think you meant d-e-f-i-n-i-t-e-l-y' 	- 6 hrs - Nil
/u/DeltaBot 	&#8710; in comment body in /r/changemyview 	keeps user delta scores @ /r/changemyview 	Active - 1 hrs - Nil
/u/Dictionary__Bot 	scans selected english words in comments 	posts definition from unknown source dictionary 	Active - 1 hrs - Nil
/u/DidSomeoneSayBoobs 	boobies in comment body 	replies with Heres some BoobiesIm Sorry  	Active - 16 mins - Nil
/u/DogeLotteryModBot 	dogecointip call in /r/DogeCoinLottery 	Lottery manager in /r/DogeCoinLottery 	Active - 1 mins - Nil
/u/DogeTipStatsBot 	** 		- 1 hrs - 54 hrs
/u/DogeWordCloudBot 	** 		- 19 mins - Nil
/u/DotaCastingBot 	** 		- 213 hrs - 159 hrs
/u/Downtotes_Plz 	** 		- 9 mins - 351 hrs
/u/DownvotesMcGoats 	** 		Active - 42 mins - 8 hrs
/u/DropBox_Bot 	** 		- 2 hrs - 443 hrs
/u/EmmaBot 	** 		- Nil - 24 hrs
/u/Epic_Face_Bot 	** 		- 2 hrs - Nil
/u/EscapistVideoBot 	** 		- 10 hrs - Nil
/u/ExmoBot 	** 		- 54 hrs - 671 hrs
/u/ExplanationBot 	** 		Active - 3 mins - Nil
/u/FTFY_Cat6 	- 	- 	deleted
/u/FTFY_Cat 	- 	- 	deleted
/u/FedoraTipAutoBot 	** 		- 2 hrs - Nil
/u/FelineFacts 	** 		- 2 hrs - Nil
/u/Fixes_GrammerNazi_ 	- 	- 	deleted
/u/FriendSafariBot 	** 		- 9 hrs - 42 hrs
/u/FriendlyCamelCaseBot 	** 		- 71 hrs - Nil
/u/FrontpageWatch 	** 		- 1 hrs - 6 mins
/u/Frown_Bot 	** 		- 7 hrs - Nil
/u/GATSBOT 	** 		- 11 hrs - Nil
/u/GabenCoinTipBot 	** 		- 19 mins - Nil
/u/GameDealsBot 	** 		- 6 hrs - Nil
/u/Gatherer_bot 	** 		- 4 hrs - Nil
/u/GeekWhackBot 	** 		- 75 hrs - Nil
/u/GiantBombBot 	Unknown (Possibly Miscellaneous) 	submitter @ /r/giantbomb 	Active - Nil - 1 hrs
/u/GifAsHTML5 	** 		- 10 mins - Nil
/u/GoneWildResearcher 	responds to pms with subject gonewild 	Searches pms for usernames in format /u/myname and says if they have posted to a gw sub. 	- 2 hrs - 7 hrs
/u/GooglePlusBot 	** 		- 4 hrs - Nil
/u/GotCrypto 	+/u/GotCrypto <value> CGB and others 	tip bot for CryptogenicBullions (see /r/CryptogenicBullion) 	Active - 3 hrs - 0 mins
/u/GrammerNazi_ 	** 		- 27 mins - Nil
/u/GreasyBacon 	- 	submitter @ /r/newonspotify 	- 41 mins - 41 mins
/u/Grumbler_bot 	** 		- Nil - Nil
/u/GunnersGifsBot 	** 		- 7 hrs - 13 hrs
/u/GunnitBot 	** 		- 5 hrs - Nil
/u/HCE_Replacement_Bot 	** 		- 39 mins - Nil
/u/HScard_display_bot 	** 		- 1 hrs - Nil
/u/Handy_Related_Sub 	detects keywords in submission in /r/guns 	suggests related subreddits 	Active - 33 mins - Nil
/u/HighResImageFinder 	** 		- 14 mins - Nil
/u/HockeyGT_Bot 	** 		- 4 hrs - 34 hrs
/u/HowIsThisBestOf_Bot 	** 		- 17 hrs - Nil
/u/IAgreeBot 	** 		- 36 hrs - Nil
/u/ICouldntCareLessBot 	** 		- 2 hrs - Nil
/u/IS_IT_SOLVED 	Unknown. Probably detects thanks/other phrases in /r/firefox 	Reminds user to flair 	Active - 41 hrs - Nil
/u/I_BITCOIN_CATS 	** 		- 1 hrs - Nil
/u/I_Say_No_ 	user-programmable triggers 	Posts No as reply 	Active - 1 mins - Nil
/u/Insane_Photo_Bot 	** 		- 4 hrs - Nil
/u/IsItDownBot 	** 		- 1 hrs - 4 hrs
/u/JiffyBot 	** 		- 1 hrs - Nil
/u/JotBot 	** 		- 3 hrs - 157 hrs
/u/JumpToBot 	** 		- 23 mins - Nil
/u/KSPortBot 	** 		- 6 hrs - Nil
/u/KarmaConspiracy_Bot 	** 		- 2 hrs - Nil
/u/LazyLinkerBot 	** 		- 6 mins - Nil
/u/LinkFixerBotSnr 	** 		- 1 hrs - Nil
/u/Link_Correction_Bot 	- 	- 	deleted
/u/Link_Demobilizer 	** 		- 51 mins - Nil
/u/Link_Rectifier_Bot 	** 		Active - 5 mins - Nil
/u/LinkedCommentBot 	** 		- 0 mins - Nil
/u/LocationBot 	** 		- 1 hrs - Nil
/u/MAGNIFIER_BOT 	** 		- 16 hrs - Nil
/u/Makes_Small_Text_Bot 	** 		- 1 hrs - Nil
/u/Meta_Bot 	- 	- 	deleted
/u/MetatasticBot 	- 	- 	deleted
/u/MetricPleaseBot 	+/u/MetricPleaseBot <query> 	returns metric units of query 	Active - 7 mins - Nil
/u/Metric_System_Bot 	** 		- 18 mins - Nil
/u/MontrealBot 	** 		- 336 hrs - 1492 hrs
/u/MovieGuide 	** 		- 16 mins - Nil
/u/MultiFunctionBot 	** 		- 12 mins - 8 mins
/u/MumeBot 	Unknown 	log and news poster @ /r/mume 	Active - Nil - Nil
/u/NASCARThreadBot 	Unknown 	Creates race discussion thread in /r/NASCAR 	Active - Nil - 95 hrs
/u/NFLVideoBot 	** 		- 1 hrs - Nil
/u/NSLbot 	** 		- 1 hrs - 1 hrs"
bot_corpus_part5 <- "/u/Nazeem_Bot 	** 		- 46 hrs - Nil
/u/New_Small_Text_Bot 	- 	- 	deleted
/u/Nidalee_Bot 	** 		- 69 hrs - 978 hrs
/u/NightMirrorMoon 	** 		- 37 mins - Nil
/u/NoSleepAutoMod 	** 		- 6 hrs - Nil
/u/NoSobStoryBot2 	** 		- 1 hrs - Nil
/u/NobodyDoesThis 	** 		- 20 hrs - Nil
/u/NotRedditEnough 	** 		- 55 hrs - Nil
/u/PHOTO_OF_CAPTAIN_RON 	** 		- 10 hrs - Nil
/u/PJRP_Bot 	** 		Active - Nil - Nil
/u/PhoenixBot 	** 		- 3 hrs - Nil
/u/PigLatinsYourComment 	** 		- 34 mins - Nil
/u/PlayStoreLinks_Bot 	** 		- 15 mins - Nil
/u/PlaylisterBot 	** 		- 1 hrs - 323 hrs
/u/PleaseRespectTables 	** 		- 47 hrs - Nil
/u/PloungeMafiaVoteBot 	** 		- 8 hrs - Nil
/u/PokemonFlairBot 	** 		- 27 hrs - 40 hrs
/u/PoliteBot 	- 	- 	deleted
/u/PoliticBot 	** 		- 1 mins - 1 mins
/u/PonyTipBot 	** 		- 1 hrs - Nil
/u/PornOverlord 	** 		- 10 mins - 720 hrs
/u/Porygon-Bot 	** 		- 38 hrs - 18 hrs
/u/PresidentObama___ 	** 		- 33 mins - Nil
/u/ProselytizerBot 	** 		- 5 hrs - Nil
/u/PunknRollBot 	** 		- 3 hrs - 279 hrs
/u/QUICHE-BOT 	** 		- 1 hrs - Nil
/u/RFootballBot 	** 		- 23 hrs - 4 mins
/u/Random-ComplimentBOT 	Unknown (Probably random) 	Replies with a random compliment 	Active - 53 mins - Nil
/u/RandomTriviaBot 	** 		- 1 hrs - Nil
/u/Rangers_Bot 	** 		- 54 hrs - 25 hrs
/u/Readdit_Bot 	** 		- 2 hrs - Nil
/u/Reads_Small_Text_Bot 	- 	- 	deleted
/u/RealtechPostBot 	** 		- 19 hrs - 9 mins
/u/ReddCoinGoldBot 	Unknown 	worker bot for /r/reddcoin 	Active - 21 mins - 47 mins
/u/Relevant_News_Bot 	** 		- 2 hrs - Nil
/u/RequirementsBot 	** 		- 43 mins - Nil
/u/RfreebandzBOT 	** 		- Nil - Nil
/u/RiskyClickBot 	** 		- 2 hrs - Nil
/u/SERIAL_JOKE_KILLER 	** 		- 6 hrs - 300 hrs
/u/SMCTipBot 	+tip SMC<value> verify 	confirms SmartCoin tip (see /r/SmartCoin) 	Active - 13 mins - Nil
/u/SRD_Notifier 	** 		- Nil - Nil
/u/SRS_History_Bot 	- 	- 	deleted
/u/SRScreenshot 	** 		Active - 1 hrs - Nil
/u/SWTOR_Helper_Bot 	** 		- 7 hrs - 31 hrs
/u/SakuraiBot_test 	** 		- 17 hrs - 10 hrs
/u/SakuraiBot 	** 		- 30 hrs - 34 hrs
/u/SatoshiTipBot 	** 		- 3 hrs - Nil
/u/ShadowBannedBot 	submission in /r/ShadowBannedBot 	replies with Youre not shadow banned! 	Active - 1 hrs - Nil
/u/ShibeBot 	- 	- 	deleted
/u/ShillForMonsanto 	** 		- 9 hrs - Nil
/u/Shiny-Bot 	** 		- 25 mins - Nil
/u/ShittyGandhiQuotes 	<something> 	replies with 	Active - 1 hrs - Nil
/u/ShittyImageBot 	** 		- 12 hrs - Nil
/u/SketchNotSkit 	** 		- 19 mins - Nil
/u/SmallTextReader 	** 		- 18 hrs - Nil
/u/Smile_Bot 	** 		- 60 mins - Nil
/u/Somalia_Bot 	** 		- 19 hrs - Nil
/u/Some_Bot 	** 		- 13 hrs - 561 hrs
/u/StackBot 	** 		- 4 hrs - Nil"
bot_corpus_part4 <- "/u/StarboundBot 	** 		- 69 hrs - 23 hrs
/u/StencilTemplateBOT 	** 		- 93 hrs - 645 hrs
/u/StreetFightMirrorBot 	video submission in /r/amateurfights and /r/StreetFights 	Creates mediafire mirror 	Active - 18 hrs - Nil
/u/SuchModBot 	** 		- 2 mins - Nil
/u/SurveyOfRedditBot 	** 		- 14 hrs - 179 hrs
/u/TOP_COMMENT_OF_YORE 	** 		- 1 hrs - Nil
/u/Text_Reader_Bot 	** 		- 2 hrs - Nil
/u/TheSwedishBot 	** 		- 262 hrs - Nil
/u/TipMoonBot 	** 		- 22 mins - 4 hrs
/u/TitsOrGTFO_Bot 	** 		- 40 mins - Nil
/u/TweetPoster 	** 		- 4 mins - Nil
/u/Twitch2YouTube 	** 		- 6 hrs - Nil
/u/Unhandy_Related_Sub 	** 		- 19 hrs - Nil
/u/UnobtaniumTipBot 	** 		- 54 hrs - Nil
/u/UrbanDicBot 	** 		- 25 hrs - Nil
/u/UselessArithmeticBot 	** 		- 27 hrs - Nil
/u/UselessConversionBot 	** 		- 17 mins - 11 hrs
/u/VideoLinkBot 	** 		- 28 mins - 327 hrs
/u/VideopokerBot 	** 		- 7 hrs - Nil
/u/VsauceBot 	New videos posted to YT channels Vsauce, Vsauce2, Vsauce3, and WeSauce 	Automatically submits videos to /r/vsauce 	Active - Nil - 27 mins
/u/WWE_Network_Bot 	** 		Active - Nil - 16 hrs
/u/WeAppreciateYou 	- 	- 	deleted
/u/Website_Mirror_Bot 	** 		- 5 hrs - Nil
/u/WeeaBot 	** 		- 21 hrs - Nil
/u/WhoWouldWinBot 	** 		- 48 mins - Nil
/u/Wiki_Bot 	** 		- 3 hrs - 24 hrs
/u/Wiki_FirstPara_bot 	** 		- 1 mins - Nil
/u/WikipediaCitationBot 	** 		- 13 hrs - Nil
/u/Wink-Bot 	** 		- 0 mins - Nil
/u/WordCloudBot2 	** 		- 20 mins - Nil
/u/WritingPromptsBot 	** 		- 15 hrs - Nil
/u/X_BOT 	** 		- Nil - Nil
/u/YT_Bot 	** 		- 9 mins - 9 hrs
/u/_Definition_Bot_ 	** 		- 14 hrs - 126 hrs
/u/_FallacyBot_ 	- 	- 	deleted
/u/_Rita_ 	** 		- 6 hrs - Nil
/u/__bot__ 	** 		- 7 hrs - Nil
/u/albumbot 	** 		- 5 mins - Nil"
bot_corpus_part3 <- "/u/allinonebot 	** 		- 14 mins - 63 hrs
/u/annoying_yes_bot 	** 		- 2 hrs - Nil
/u/asmrspambot 	** 		- 3 hrs - Nil
/u/astro-bot 	** 		- 28 hrs - 497 hrs
/u/auto-doge 	Top submission from subreddit 	posts 	Active - 10 mins - Nil
/u/automoderator 	** 		- 0 mins - 1 hrs
/u/autourbanbot 	** 		- 42 mins - Nil
/u/autowikibot 	** 		- 1 mins - Nil
/u/bRMT_Bot 	** 		- 4 hrs - Nil
/u/bad_ball_ban_bot 	** 		- 7 hrs - Nil
/u/ban_pruner 	** 		- Nil - Nil
/u/baseball_gif_bot 	** 		- 54 hrs - Nil
/u/beecointipbot 	+/u/beecointipbot <value> 	tip bot of beecoin (see /r/beecoin) 	Active - 2 mins - 42 mins
/u/bitcoinpartybot 	** 		- 31 hrs - Nil
/u/bitcointip 	** 		- 41 mins - Nil
/u/bitofnewsbot 	** 		- 1 hrs - Nil
/u/bocketybot 	Unknown (probably detects words suggestive of visit to ireland in submission in /r/ireland) 	Suggests /r/irishtourism 	Active - 125 hrs - Nil
/u/c5bot 	** 		- 37 hrs - 23 hrs
/u/c5bot 	Miscellaneous 	Worker at /r/concrete5 	Active - 47 hrs - 23 hrs
/u/cRedditBot 	** 		- 1 hrs - Nil
/u/callfloodbot 	** 		- 2 hrs - 2 hrs
/u/callibot 	Unknown (probably scheduled) 	Submission bot for /r/Calligraphy 	Active - 1068 hrs - 16 hrs
/u/canada_goose_tip_bot 	** 		- 43 hrs - Nil
/u/changetip 	** 		Active - 2 hrs - 125 hrs
/u/cheesecointipbot 	** 		- 47 mins - Nil
/u/chromabot 	** 		- 2 hrs - 74 hrs
/u/classybot 	** 		- 1 hrs - Nil"

bot_corpus_part2 <- "/u/coinflipbot 	** 		- 1 hrs - Nil
/u/coinyetipper 	** 		- 46 mins - Nil
/u/colorcodebot 	** 		- 47 mins - Nil
/u/comment_copier_bot 	** 		- 244 hrs - Nil
/u/compilebot 	** 		- 4 hrs - Nil
/u/conspirobot 	** 		- 0 mins - 5 mins
/u/creepiersmilebot 	** 		- 40 mins - Nil
/u/cris9696 	linkme: <android app name> in specific android-related subs 	Gives direct and search link to playstore 	Active - 38 mins - 69 hrs
/u/cruise_bot 	** 		- 4 hrs - Nil
/u/d3posterbot 	** 		- 19 hrs - 129 hrs
/u/define_bot 	** 		- 22 hrs - 63 hrs
/u/demobilizer 	** 		- 2 hrs - Nil
/u/dgctipbot 	** 		Active - Nil - Nil
/u/digitipbot 	** 		- 1 hrs - 5 mins
/u/disapprovalbot 	** 		- 33 hrs - Nil
/u/dogetipbot 	** 		- 0 mins - 343 hrs
/u/earthtipbot 	** 		- 2 hrs - Nil
/u/edmprobot 	** 		- 636 hrs - 52 hrs
/u/elMatadero_bot 	Unknown 	submitter @ /r/elMatadero 	Active - 0 mins - 1 hrs
/u/elwh392 	- 	- 	deleted
/u/expired_link_bot 	** 		- 1 hrs - Nil
/u/fa_mirror 	Link to furaffinity.net content in submission 	gives imgur mirror and miscellaneous info 	Active - 11 hrs - 283 hrs
/u/fact_check_bot 	** 		- 1 hrs - Nil
/u/faketipbot 	+/u/faketipbot <value> 	fake verify comment reply 	Active - Nil - Nil
/u/fedora_tip_bot 	** 		- 1 hrs - 158 hrs
/u/fedoratips 	** 		- 11 mins - Nil
/u/flappytip 	+/u/flappytip <value> flaps 	tip bot for flappycoin (see /r/flappycoin) 	Active - 15 mins - 30 mins
/u/flips_title 	- 	- 	deleted
/u/foreigneducationbot 	** 		- 40 mins - Nil
/u/frytipbot 	+/u/frytipbot <value> fry 	tip bot for frycoin (see /r/Frycoin) 	Active - 14 mins - Nil
/u/fsctipbot 	+/u/fsctipbot <value> fsc 	tip bot for Friendshipcoin (see /r/friendshipcoin) 	Active - 1 hrs - Nil
/u/gabenizer-bot 	** 		- 149 hrs - 2 hrs
/u/gabentipbot 	** 		- Nil - Nil
/u/gfy_bot 	** 		- 17 mins - Nil
/u/gfycat-bot-sucksdick 	** 		- 0 mins - Nil
/u/gifster_bot 	** 		- 12 hrs - Nil
/u/gives_you_boobies 	** 		- 6 hrs - Nil
/u/givesafuckbot 	** 		- Nil - Nil
/u/gocougs_bot 	** 		- 31 hrs - Nil
/u/godwin_finder 	** 		- 50 mins - Nil
/u/golferbot 	** 		- 49 hrs - Nil
/u/gracefulcharitybot 	** 		- 1 hrs - Nil
/u/gracefulclaritybot 	** 		- 12 hrs - Nil
/u/gregbot 	** 		- 15 hrs - 427 hrs
/u/groompbot 	** 		- Nil - 9 hrs
/u/gunners_gif_bot 	** 		- 27 hrs - Nil
/u/haiku_robot 	** 		- 1 hrs - Nil
/u/havoc_bot 	** 		- 14 mins - 14 mins
/u/hearing-aid_bot 	** 		- 42 hrs - Nil
/u/hearing_aid_bot 	** 		- 1 hrs - Nil
/u/hearingaid_bot 	** 		- 5 mins - Nil
/u/hit_bot 	** 		- 11 mins - 746 hrs
/u/hockey_gif_bot 	** 		- 3 hrs - Nil
/u/howstat 	howstat <player_name first, followed by optional filters, all separated by commas> 	Statistics bot for /r/Cricket 	Active - 7 hrs - Nil
/u/hwsbot 	** 		- 2 hrs - 317 hrs
/u/imgurHostBot 	** 		- 4 hrs - Nil
/u/imgur_rehosting 	** 		- 50 mins - Nil
/u/imgurtranscriber 	** 		- 1 mins - Nil
/u/imirror_bot 	** 		- 6 hrs - Nil
/u/isitupbot 	** 		- 2 hrs - Nil
/u/jerkbot-3hunna 	submission in /r/Hiphopcirclejerk 	Screenshot of linked reddit page 	Active - 3 hrs - Nil
/u/keysteal_bot 	** 		- Nil - Nil
/u/kittehcointipbot 	** 		- 1 hrs - -56 mins
/u/last_cakeday_bot 	** 		Active - 32 mins - Nil
/u/linkfixerbot1 	- 	- 	deleted
/u/linkfixerbot2 	** 		- 2 hrs - Nil
/u/linkfixerbot3 	** 		- Nil - Nil
/u/loser_detector_bot 	** 		- 46 mins - Nil
/u/luckoftheshibe 	10 minute cycle - random comment in /r/CryptoCraft 	random comment chooser @ /r/CryptoCraft 	Active - 30 mins - Nil
/u/makesTextSmall 	** 		- 50 mins - Nil
/u/malen-shutup-bot 	** 		- 1 hrs - Nil
/u/matthewrobo 	Specific thread in /r/ChooseTheAdventure 	Adventure game AI 	Active - 1 hrs - 127 hrs
/u/meme_transcriber 	** 		- 5 hrs - Nil
/u/memedad-transcriber 	** 		- 18 mins - Nil
/u/misconception_fixer 	** 		- 11 mins - Nil
/u/mma_gif_bot 	** 		- 11 hrs - Nil
/u/moderator-bot 	** 		- 1 hrs - 55 hrs
/u/nba_gif_bot 	** 		- 3 hrs - Nil
/u/new_eden_news_bot 	** 		- 17 hrs - 9 hrs
/u/nhl_gif_bot 	gif submission in /r/nhl 	Creates compressed version of gif 	Active - 102 hrs - Nil
/u/not_alot_bot 	** 		- 2 hrs - Nil
/u/notoverticalvideo 	** 		- 1 hrs - Nil
/u/nyantip 	** 		- 20 mins - 5 hrs
/u/okc_rating_bot 	** 		- 25 mins - 395 hrs
/u/pandatipbot 	** 		- 5 mins - Nil
/u/pandatips 	+/u/pandatips <value> bamboo verify 	tipbot for pandacoin see /r/therealpandacoin 	Active - 21 mins - Nil
/u/potdealer 	** 		- 20 mins - Nil
/u/provides-id 	submissions in selected subreddits 	Provides IDs of celebrities 	Active - 25 hrs - Nil
/u/qznc_bot 	** 		- 22 mins - 21 mins
/u/rSGSpolice 	** 		- 58 mins - Nil
/u/r_PictureGame 	** 		Active - 2 hrs - Nil
/u/raddit-bot 	** 		- 15 mins - 11 mins
/u/randnumbot 	** 		- 2 hrs - Nil
/u/rarchives 	** 		- 1 hrs - Nil
/u/readsmalltextbot 	** 		- 59 mins - Nil
/u/redditbots 	** 		- 13 mins - 406 hrs
/u/redditreviewbot 	** 		- 197 hrs - Nil
/u/redditreviewbot 	Unknown 	service bot for /r/GroupSRC 	Active - 284 hrs - Nil
/u/reddtipbot 	** 		- 4 mins - Nil
/u/relevantxkcd-bot 	** 		- 13 mins - Nil
/u/request_bot 	** 		- 3 hrs - 1819 hrs
/u/rhiever-bot 	** 		- 9 hrs - Nil
/u/rightsbot 	** 		- 1 hrs - 1 hrs
/u/rnfl_robot 	** 		- 12 hrs - Nil
/u/roger_bot 	** 		- 30 mins - Nil
/u/rss_feed 	** 		- 54 hrs - 3 hrs
/u/rubycointipbot 	** 		Active - 2 hrs - 10 hrs
/u/rule_bot 	** 		- 7 mins - Nil
/u/rusetipbot 	** 		- 36 hrs - Nil
/u/sentimentviewbot 	** 		- 86 hrs - Nil
/u/serendipitybot 	** 		- 1 hrs - 2 hrs
/u/shadowbanbot 	** 		- 4 hrs - Nil
/u/slapbot 	!slap 	Replies with /u/parentcommenter slaps /u/grandparentcommenter around a bit with a large trout 	Active - 6 mins - Nil
/u/slickwom-bot 	** 		- 3 hrs - Nil
/u/snapshot_bot 	/u/snapshot_bot <URL> 	Creates snapshot of webpage and posts as imgur image+album 	Active - 24 hrs - Nil
/u/soccer_gif_bot 	** 		- 2 hrs - Nil
/u/softwareswap_bot 	** 		Active - 0 mins - Nil
/u/sports_gif_bot 	** 		Active - Nil - Nil
/u/spursgifs_xposterbot 	** 		- 19 hrs - 37 hrs
/u/stats-bot 	** 		- 1550 hrs - Nil
/u/steam_bot 	** 		- Nil - 384 hrs
/u/subtext-bot 	** 		- 23 hrs - Nil
/u/synonym_flash 	** 		- 5 hrs - Nil
/u/tabledresser 	** 		- 10 hrs - 2 hrs
/u/techobot 	** 		- 6 hrs - 29 hrs
/u/tennis_gif_bot 	** 		- 39 hrs - Nil
/u/test_bot0x00 	** 		- 1 hrs - Nil
/u/tipmoonbot1 	** 		- 3 hrs - Nil
/u/tipmoonbot2 	** 		- 6 hrs - Nil
/u/tittietipbot 	** 		- 7 hrs - 2 mins
/u/topcoin_tip 	** 		- 37 mins - 2 hrs
/u/topredditbot 	** 	
/u/totes_meta_bot 	reddit comment link in another comment 	informs linked thread about metalink 	Active - 6 mins - Nil
/u/ttumblrbots 	** 		- 18 mins - Nil
/u/unitconvert 	** 		- Nil - Nil
/u/valkyribot 	** 		- 7 hrs - 8 hrs
/u/versebot 	** 		- 33 mins - Nil
/u/vertcoinbot 	- 	moderator @ /r/vertcoin 	Active - 3 mins - Nil
/u/vertcointipbot 	** 		- 25 mins - 24 mins
/u/wheres_the_karma_bot 	** 		- 5 hrs - Nil
/u/wooshbot 	** 		- 200 hrs - Nil
/u/xkcd_bot 	** 		- 36 hrs - Nil
/u/xkcd_number_bot 	** 		- 5 hrs - Nil
/u/xkcd_number_bot 	** 		Active - 50 hrs - Nil
/u/xkcd_number_bot 	** 		Active - 50 hrs - Nil
/u/xkcd_transcriber 	** 		- 22 mins - 191 hrs
/u/xkcdcomic_bot 	** 		- 78 hrs - 23 hrs
/u/yes_it_is_weird 	** 		- 34 mins - Nil
/u/yourebot 	** 		- Nil - Nil"


# Regular Expression Go!!! -----------------------------------------------------------

#This should detect if it starts wit /u/ but not select it, and then select all until a whitespace (yes it would have been faster, if I just typed it by hand)
skynet <- str_extract_all(bots_corpus,"(?<=\\/u\\/)([^\\s]+)")
q1 <- as.data.frame.list(skynet, header=FALSE)
names(q1)[1] <- "Botnames"


skynet2 <- str_extract_all(bot_corpus_part2,"(?<=\\/u\\/)([^\\s]+)")
q2 <- as.data.frame.list(skynet2, header=FALSE)
names(q2)[1] <- "Botnames"

skynet3 <- str_extract_all(bot_corpus_part3,"(?<=\\/u\\/)([^\\s]+)")
q3 <- as.data.frame.list(skynet3, header=FALSE)
names(q3)[1] <- "Botnames"

skynet4 <- str_extract_all(bot_corpus_part4,"(?<=\\/u\\/)([^\\s]+)")
q4 <- as.data.frame.list(skynet4, header=FALSE)
names(q4)[1] <- "Botnames"

skynet5 <- str_extract_all(bot_corpus_part5,"(?<=\\/u\\/)([^\\s]+)")
q5 <- as.data.frame.list(skynet5, header=FALSE)
names(q5)[1] <- "Botnames"

q1 <- bind_rows(q1,q2)
q1 <- bind_rows(q1,q3)
q1 <- bind_rows(q1,q4)
q1 <- bind_rows(q1,q5)



write_csv(q1,paste0(masterarbeit_code_filepath,"botlist.csv"))
write_csv(q1,paste0(reddit_data_filepath,"botlist.csv"))

