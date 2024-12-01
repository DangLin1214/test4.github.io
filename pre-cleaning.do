
use "./data/ukb_data.dta", clear

* pre-cleaning codebook
/*
	rename variables
	create secondary variables based on primary variables (eg diet_quality)
*/

* rename main variables
********************************************************************************

ren n_21022_0_0		age
ren n_31_0_0		sex
ren n_21000_0_0		ethnicity
ren n_189_0_0		townsend
ren n_6177_0_0		medication
ren n_738_0_0		income
ren n_20107_0_0		father_ill
ren n_20110_0_0		mother_ill
ren n_29000_0_0		mental_health
ren n_29088_0_0		marriage

ren s_53_0_0		baseline_date
ren s_191_0_0		lost_date
ren s_40000_0_0		death_date
ren s_40022_0_0		record_origin
ren s_40020_0_0		death_origin

********************************************************************************

** ethnicity

gen ethnicity_cat = 0
replace ethnicity_cat = 1 if ethnicity == 1001 | ethnicity == 1002 | ethnicity == 1003
tab ethnicity_cat

** physical activity

ren n_22037_0_0		MET_walk
ren n_22038_0_0		MET_moderate
ren n_22039_0_0		MET_vigorous
gen total_MET = MET_moderate + MET_vigorous
gen pa_cat = .
replace pa_cat = 100 if total_MET >= 150
replace pa_cat = 90 if total_MET < 150 & total_MET >= 120
replace pa_cat = 80 if total_MET < 120 & total_MET >= 90
replace pa_cat = 60 if total_MET < 90 & total_MET >= 60
replace pa_cat = 40 if total_MET < 60 & total_MET >= 30
replace pa_cat = 20 if total_MET < 30 & total_MET > 0
replace pa_cat = 0 if total_MET <= 0

** diet

****** vegetable ******
ren n_1289_0_0 cooked_vegetable_0
gen cooked_vegetable=cooked_vegetable_0 if cooked_vegetable_0!=-3 & cooked_vegetable_0!=-1 & cooked_vegetable_0!=-10
replace cooked_vegetable=0.5 if cooked_vegetable_0==-10
replace cooked_vegetable= . if cooked_vegetable_0==-3 | cooked_vegetable_0==-1
label variable cooked_vegetable "Cooked vegetable intake(tablespoons/d)"

ren n_1299_0_0 salad_raw_vegetable_0
gen salad_raw_vegetable=salad_raw_vegetable_0 if salad_raw_vegetable_0!=-3 & salad_raw_vegetable_0!=-1 & salad_raw_vegetable_0!=-10
replace salad_raw_vegetable=0.5 if salad_raw_vegetable_0==-10
replace salad_raw_vegetable= . if salad_raw_vegetable_0==-3 | salad_raw_vegetable_0==-1
label variable salad_raw_vegetable "Salad / raw vegetable intake intake(tablespoons/d)"

* 1 serving=3 tablespoons
gen healthy_vegetable = 1 if ((cooked_vegetable+salad_raw_vegetable)/3>=3 & cooked_vegetable!= . & salad_raw_vegetable!= .) | (cooked_vegetable/3>=3 & cooked_vegetable!= . ) | (salad_raw_vegetable/3>=3 & salad_raw_vegetable!= .)
replace healthy_vegetable = 0 if healthy_vegetable == . & (cooked_vegetable!= . | salad_raw_vegetable!= .)
label variable healthy_vegetable "vegetables: ≥ 3 servings/day"

****** fruit ******
ren n_1309_0_0 fresh_fruit_0
gen fresh_fruit=fresh_fruit_0 if fresh_fruit_0!=-3 & fresh_fruit_0!=-1 & fresh_fruit_0!=-10
replace fresh_fruit=0.5 if fresh_fruit_0==-10
replace fresh_fruit= . if fresh_fruit_0==-3 | fresh_fruit_0==-1
label variable fresh_fruit "Fresh fruit intake(pieces/d)"

ren n_1319_0_0 dried_fruit_0
gen dried_fruit=dried_fruit_0 if dried_fruit_0!=-3 & dried_fruit_0!=-1 & dried_fruit_0!=-10
replace dried_fruit=0.5 if dried_fruit_0==-10
replace dried_fruit= . if dried_fruit_0==-3 | dried_fruit_0==-1
label variable dried_fruit "Dried fruit intake(pieces/d)"

* 1 serving=5 pieces
gen healthy_fruit = 1 if (fresh_fruit+dried_fruit/5>=3 & fresh_fruit!= . & dried_fruit!= .) | (fresh_fruit>=3 & fresh_fruit!= . ) | (dried_fruit/5>=3 & dried_fruit!= .)
replace healthy_fruit = 0 if healthy_fruit == . & (fresh_fruit!= . | dried_fruit!= .)
label variable healthy_fruit "Fruits: ≥ 3 servings/day"

****** fish ******
ren n_1329_0_0 oily_fish_0
recode oily_fish_0 (-3/-1=-1 "Do not know/Prefer not to answer") (0=0 "Never") (1=1 "Less than once a week") (2=2 "Once a week") (3=3 "2-4 times a week") (4=4 "5-6 times a week") (5=5 "Once or more daily"), gen(oily_fish)
replace oily_fish= . if oily_fish==-1
label variable oily_fish "Oily fish intake"

ren n_1339_0_0 non_oily_fish_0
recode non_oily_fish_0 (-3/-1=-1 "Do not know/Prefer not to answer") (0=0 "Never") (1=1 "Less than once a week") (2=2 "Once a week") (3=3 "2-4 times a week") (4=4 "5-6 times a week") (5=5 "Once or more daily"), gen(non_oily_fish)
replace non_oily_fish= . if non_oily_fish==-1
label variable non_oily_fish "Non-oily fish intake"

gen healthy_fish = 1 if ((oily_fish+non_oily_fish)>=4 & oily_fish!= . & non_oily_fish!= .) | (oily_fish>=3 & oily_fish!= . ) | (non_oily_fish>=3 & non_oily_fish!= .)
replace healthy_fish = 0 if healthy_fish == . & (oily_fish!= . | non_oily_fish!= .)
label variable healthy_fish "fish: >=2 servings/week"

****** processed meat ******
ren n_1349_0_0 processed_meat_0
recode processed_meat_0 (-3/-1=-1 "Do not know/Prefer not to answer") (0=0 "Never") (1=1 "Less than once a week") (2=2 "Once a week") (3=3 "2-4 times a week") (4=4 "5-6 times a week") (5=5 "Once or more daily"), gen(processed_meat)
replace processed_meat= . if processed_meat==-1
label variable processed_meat "Processed meat intake"

gen healthy_processed_meat=1 if processed_meat<=2 & processed_meat!= .
replace healthy_processed_meat = 0 if healthy_processed_meat== . & processed_meat!= .
label variable healthy_processed_meat "processed meat: ≤1 serving/week"

****** red meat ******
ren n_1369_0_0 beef_0
recode beef_0 (-3/-1=-1 "Do not know/Prefer not to answer") (0=0 "Never") (1=1 "Less than once a week") (2=2 "Once a week") (3=3 "2-4 times a week") (4=4 "5-6 times a week") (5=5 "Once or more daily"), gen(beef)
replace beef= . if beef==-1
label variable beef "Beef intake"

ren n_1379_0_0 lamb_mutton_0
recode lamb_mutton_0 (-3/-1=-1 "Do not know/Prefer not to answer") (0=0 "Never") (1=1 "Less than once a week") (2=2 "Once a week") (3=3 "2-4 times a week") (4=4 "5-6 times a week") (5=5 "Once or more daily"), gen(lamb_mutton)
replace lamb_mutton= . if lamb_mutton==-1
label variable lamb_mutton "Lamb/mutton intake"

ren n_1389_0_0 pork_0
recode pork_0 (-3/-1=-1 "Do not know/Prefer not to answer") (0=0 "Never") (1=1 "Less than once a week") (2=2 "Once a week") (3=3 "2-4 times a week") (4=4 "5-6 times a week") (5=5 "Once or more daily"), gen(pork)
replace pork= . if pork==-1
label variable pork "Pork intake"

gen healthy_red_meat=1 if beef + lamb_mutton + pork <= 3 & beef!= . & lamb_mutton!= . & pork!= . & beef<3 & lamb_mutton<3 & pork<3 
replace healthy_red_meat = 0 if healthy_red_meat == . & (beef!= . | lamb_mutton!= . | pork!= .)
label variable healthy_red_meat "red meat: ≤1.5 serving/week"

****** whole grain & refined grain ******
ren n_1438_0_0 bread_0
recode bread_0 (-3/-1=-1 "Do not know/Prefer not to answer") (-10=-10 "Less than one"), gen(bread)
replace bread= . if bread==-1
replace bread=0.5 if bread==-10
label variable bread "Bread intake(slices/week)"

ren n_1448_0_0 bread_type_0
recode bread_type_0 (-3/-1=-1 "Do not know/Prefer not to answer") (1=1 "White") (2=2 "Brown") (3=3 "Wholemeal or wholegrain") (4=4 "Other type of bread"), gen(bread_type)
replace bread_type= . if bread_type==-1
label variable bread_type "Bread type"

ren n_1458_0_0 cereal_0
recode cereal_0 (-3/-1=-1 "Do not know/Prefer not to answer") (-10=-10 "Less than one"), gen(cereal)
replace cereal= . if cereal==-1
replace cereal=0.5 if cereal==-10
label variable cereal "Cereal intake(bowls/week)"

ren n_1468_0_0 cereal_type_0
recode cereal_type_0 (-3/-1=-1 "Do not know/Prefer not to answer") (1=1 "Bran cereal (e.g. All Bran, Branflakes)") (2=2 "Biscuit cereal (e.g. Weetabix)") (3=3 "Oat cereal (e.g. Ready Brek, porridge)") (4=4 "Muesli") (5=5 "Other (e.g. Cornflakes, Frosties)"), gen(cereal_type)
replace cereal_type= . if cereal_type==-1
label variable cereal_type "cereal type"

gen healthy_refined_grain=0 if (bread>1.5 & (bread_type==1 |bread_type==2 |bread_type==4) & bread!=. & bread_type!=.) | (cereal>1.5 & (cereal_type==2 |cereal_type==5) & cereal!=. & cereal_type!=.) | (bread+cereal>1.5 & (bread_type==1 |bread_type==2 |bread_type==4) & bread!=. & bread_type!=. & (cereal_type==2 |cereal_type==5) & cereal!=. & cereal_type!=.)
replace healthy_refined_grain=1 if bread_type==3 &(cereal_type==1 |cereal_type==3 |cereal_type==4)
replace healthy_refined_grain=1 if (bread<=1.5 & bread!=. & (cereal_type==1|cereal_type==3|cereal_type==4)) | (cereal<=1.5 & cereal!=. & (bread_type==3))
replace healthy_refined_grain=1 if bread+cereal<=1.5 & bread!=. & cereal!=.
label variable healthy_refined_grain "refined grain: ≤1.5 serving/week"

gen healthy_whole_grain=1 if (bread>=3 & bread_type==3 & bread!=. & bread_type!=.) | (cereal>=3 & (cereal_type==1 |cereal_type==3 |cereal_type==4) & cereal!=. & cereal_type!=.) | (bread+cereal>=3 & bread_type==3 & bread!=. & bread_type!=. & (cereal_type==1 |cereal_type==3|cereal_type==4) & cereal!=. & cereal_type!=.)
replace healthy_whole_grain=0 if (bread_type==1|bread_type==2|bread_type==4) &(cereal_type==2 |cereal_type==5)
replace healthy_whole_grain=0 if (bread<3 & bread!=. & (cereal_type==2|cereal_type==5)) | (cereal<3 & cereal!=. & (bread_type==1|bread_type==2|bread_type==4))
replace healthy_whole_grain=0 if bread+cereal<3 & bread!=. & cereal!=.
label variable healthy_whole_grain "whole grain: >=3 serving/week"

drop cooked_vegetable_0 salad_raw_vegetable_0 cooked_vegetable salad_raw_vegetable fresh_fruit_0 dried_fruit_0 oily_fish_0 non_oily_fish_0 

egen diet_quality = rowtotal(healthy_vegetable healthy_fruit healthy_fish healthy_processed_meat healthy_red_meat healthy_whole_grain healthy_refined_grain)
egen diet_quality_cat_0 = cut(diet_quality), group(20)
recode diet_quality_cat_0 (0 1 = 0) (2 3 4 5 = 20) (6 7 8 9 = 40) (10 11 12 13 = 60) (14 15 16 = 80) (17 18 = 90) (19 = 100), gen(diet_quality_cat)


** sleep

ren n_1160_0_0		sleep_hour
replace sleep_hour = . if sleep_hour == -1 | sleep_hour == -3
gen sleep_cat = .
replace sleep_cat = 25 if sleep_hour >= 7 & sleep_hour < 9
replace sleep_cat = 20 if sleep_hour >= 9 & sleep_hour < 10
replace sleep_cat = 15 if sleep_hour >= 6 & sleep_hour < 7
replace sleep_cat = 10 if sleep_hour >= 5 & sleep_hour < 6 | sleep_hour >= 10
replace sleep_cat = 5 if sleep_hour >= 4 & sleep_hour < 5
replace sleep_cat = 0 if sleep_hour < 4


****** chronotype ******
recode n_1180_0_0 (-3/-1=-1 "Do not know/Prefer not to answer") (1=1 "Definitely a morning") (2=2 "More a morning") (3=3 "More an evening") (4=4 "Definitely an evening"), gen(Chronotype_preference_0)
replace Chronotype_preference_0 = . if Chronotype_preference_0 == -1				   
gen chronotype=1 if Chronotype_preference_0==1 | Chronotype_preference_0 == 2
replace chronotype=0 if Chronotype_preference_0==. & Chronotype_preference_0 !=. 

****** insomnia ******
recode n_1200_0_0 (-3=-3 "Prefer not to answer") (1=1 "Never/rarely") (2=2 "Sometimes") (3=3 "Usually"), gen(Insomnia_0)  
replace Insomnia_0=. if Insomnia_0==-3
gen insomnia=1 if Insomnia_0==1
replace insomnia=0 if Insomnia==. & Insomnia_0 !=.

****** snoring ******
recode n_1210_0_0 (-3/-1=-1 "Prefer not to answer") (1=1 "yes") (2=2 "no"),gen(snoring_0)
replace snoring_0=. if snoring_0==-1
gen snoring=1 if snoring_0==2
replace snoring=0 if snoring==. & snoring_0!=.

****** daytime_sleepiness ******
recode n_1220_0_0 (-3/-1=-1 "Prefer not to answer") (0=0 "Never/rarely") (1=1 "Sometimes") (2=2 "often") (3=3 "All of the time"), gen(daytime_sleepiness_0)    
replace daytime_sleepiness_0=. if daytime_sleepiness_0==-1
gen daytime_sleepiness=1 if daytime_sleepiness_0==0 | daytime_sleepiness_0==1
replace daytime_sleepiness=0 if daytime_sleepiness==. & daytime_sleepiness_0!=.

drop Chronotype_preference_0 Insomnia_0 snoring_0 daytime_sleepiness_0
egen sleep_quality = rowtotal(chronotype insomnia snoring daytime_sleepiness)
replace sleep_cat = sleep_cat * sleep_quality
drop if sleep_cat == .

** smoke

ren n_1239_0_0		smoke_current
ren n_1249_0_0		smoke_past
replace smoke_current = . if smoke_current == -3
replace smoke_past = . if smoke_past == -3
gen smoke_cat = .
replace smoke_cat = 100 if smoke_current == 0 & smoke_past == 4
replace smoke_cat = 90 if smoke_current == 0 & smoke_past == 3
replace smoke_cat = 75 if smoke_current == 0 & smoke_past == 2
replace smoke_cat = 50 if smoke_current == 0 & smoke_past == 1
replace smoke_cat = 25 if smoke_current == 2
replace smoke_cat = 0 if smoke_current == 1
drop if smoke_cat == .

** alcohol

replace n_1568_0_0 = . if n_1568_0_0 == -1 | n_1568_0_0 == -3
gen redwine_week = n_1568_0_0 * 1.5
replace n_4407_0_0 = . if n_4407_0_0 == -1 | n_4407_0_0 == -3
gen redwine_month = n_4407_0_0 * 1.5 / 4

replace n_1578_0_0 = . if n_1578_0_0 == -1 | n_1578_0_0 == -3
gen champagne_week = n_1578_0_0 * 1.5
replace n_4418_0_0 = . if n_4418_0_0 == -1 | n_4418_0_0 == -3
gen champagne_month = n_4418_0_0 * 1.5 / 4

replace n_1588_0_0 = . if n_1588_0_0 == -1 | n_1588_0_0 == -3
gen beer_week = n_1588_0_0 * 2
replace n_4429_0_0 = . if n_4429_0_0 == -1 | n_4429_0_0 == -3
gen beer_month = n_4429_0_0 * 2 / 4

replace n_1598_0_0 = . if n_1598_0_0 == -1 | n_1598_0_0 == -3
gen spirits_week = n_1598_0_0 * 1
replace n_4440_0_0 = . if n_4440_0_0 == -1 | n_4440_0_0 == -3
gen spirits_month = n_4440_0_0 * 1 / 4

replace n_1608_0_0 = . if n_1608_0_0 == -1 | n_1608_0_0 == -3
gen wine_week = n_1608_0_0 * 1.5
replace n_4451_0_0 = . if n_4451_0_0 == -1 | n_4451_0_0 == -3
gen wine_month = n_4451_0_0 * 1.5 / 4

egen total_alcohol = rowtotal(redwine_week redwine_month champagne_week champagne_month beer_week beer_month spirits_week spirits_month wine_week wine_month)
gen alcohol_cat = .
replace alcohol_cat = 100 if total_alcohol < 22 & sex == 1
replace alcohol_cat = 75 if total_alcohol >= 22 & total_alcohol < 44 & sex == 1
replace alcohol_cat = 50 if total_alcohol >= 44 & total_alcohol < 66 & sex == 1
replace alcohol_cat = 25 if total_alcohol >= 66 & total_alcohol < 88 & sex == 1
replace alcohol_cat = 0 if total_alcohol >= 100 & sex == 1
replace alcohol_cat = 100 if total_alcohol < 15 & sex == 0
replace alcohol_cat = 75 if total_alcohol >= 15 & total_alcohol < 30 & sex == 0
replace alcohol_cat = 50 if total_alcohol >= 30 & total_alcohol < 45 & sex == 0
replace alcohol_cat = 25 if total_alcohol >= 45 & total_alcohol < 60 & sex == 0
replace alcohol_cat = 0 if total_alcohol >= 60 & sex == 0
drop if alcohol_cat == .

** blood lipids

ren n_23400_0_0		tc
ren n_23406_0_0		hdl
gen non_hdl = tc * 88.6 - hdl * 38.6
gen non_hdl_cat = .
replace non_hdl_cat = 100 if non_hdl < 130
replace non_hdl_cat = 90 if non_hdl >= 130 & non_hdl < 140
replace non_hdl_cat = 80 if non_hdl >= 140 & non_hdl < 160
replace non_hdl_cat = 60 if non_hdl >= 160 & non_hdl < 180
replace non_hdl_cat = 40 if non_hdl >= 180 & non_hdl < 200
replace non_hdl_cat = 20 if non_hdl >= 200 & non_hdl < 220
replace non_hdl_cat = 0 if non_hdl >= 220

** blood glucose

ren n_30740_0_0		tg_0
ren n_30750_0_0		hba1c
gen tg = tg_0 * 18
gen glu_cat = .
replace glu_cat = 100 if hba1c < 57
replace glu_cat = 80 if hba1c >= 57 & hba1c < 64
replace glu_cat = 60 if hba1c >= 57 & hba1c < 64
replace glu_cat = 40 if hba1c >= 64 & hba1c < 70
replace glu_cat = 30 if hba1c >= 70 & hba1c < 80
replace glu_cat = 20 if hba1c >= 80 & hba1c < 90
replace glu_cat = 10 if hba1c >= 90 & hba1c < 100
replace glu_cat = 0 if hba1c >= 100

** blood pressure

ren n_4079_0_0		dbp
ren n_4080_0_0		sbp
gen bp_cat = .
replace bp_cat = 100 if sbp < 120 & dbp < 80
replace bp_cat = 80 if (sbp >= 120 & sbp < 130) & dbp < 80
replace bp_cat = 60 if (sbp >= 130 & sbp < 140)|(dbp >= 80 & dbp < 90)
replace bp_cat = 40 if (sbp >= 140 & sbp < 160)|(dbp >= 90 & dbp < 100)
replace bp_cat = 20 if (sbp >= 160 & sbp < 180)|(dbp >= 100 & dbp < 110)
replace bp_cat = 0 if (sbp >= 180)|(dbp >= 110)

save "./data/inf_0.csv", replace
