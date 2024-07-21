library("readxl")
Polity5_1 <- read_excel("/Users/ilia_nad/Desktop/R_EU//p5v2018d.xls")
names(Polity5_1)
#Расчет медианы, среднего арифметического, минимума и максимума для всех случаев ====
Bratya_slavyaneNumbers<-subset(Polity5_1, (Polity5_1$country=="Russia"|Polity5_1$country=="Belarus"|Polity5_1$country=="Ukraine") & Polity5_1$byear>=1992& Polity5_1$polity >=-10)
Bratya_slavyaneNumbers
tapply(Bratya_slavyaneNumbers$polity, Bratya_slavyaneNumbers$country, summary,na.rm = T)
tapply(Bratya_slavyaneNumbers$polity, Bratya_slavyaneNumbers$country, sd,na.rm = T)
#Мода России + альтернативный способ расчета для России ==== 
Russia_polity<-subset(Polity5_1, Polity5_1$country=="Russia" & Polity5_1$byear >=1992& Polity5_1$polity >=-10)
Russia_polity
Russia_polity_1992<-subset(Russia_polity, Russia_polity$country=="Russia","polity") 
Russia_polity_1992
summary(Russia_polity_1992)
sd(Russia_polity_1992$polity)
Modes <- function(Russia_polity_1992) {      
  unique_values <- unique(Russia_polity_1992)
  tabulated_values <- tabulate(match(Russia_polity_1992, unique_values))
  all_modes <- unique_values[tabulated_values == max(tabulated_values)]
  return(all_modes)
  }
Modes(Russia_polity_1992$polity)

#Мода Украины + альтернативный способ расчета для Украины ==== 
Ukraine_polity<-subset(Polity5_1, Polity5_1$country=="Ukraine" & Polity5_1$byear >=1992 & Polity5_1$polity >=-10)
Ukraine_polity
Ukraine_polity_1992<-subset(Ukraine_polity, Ukraine_polity$country=="Ukraine","polity") 
Ukraine_polity_1992
summary(Ukraine_polity_1992)
sd(Ukraine_polity_1992$polity)
Modes <- function(Ukraine_polity_1992) {      
  unique_values <- unique(Ukraine_polity_1992)
  tabulated_values <- tabulate(match(Ukraine_polity_1992, unique_values))
  all_modes <- unique_values[tabulated_values == max(tabulated_values)]
  return(all_modes)
}
Modes(Ukraine_polity_1992$polity)

#Мода Беларуси + альтернативный способ расчета для Беларуси ==== 
Belarus_polity<-subset(Polity5_1, Polity5_1$country=="Belarus" & Polity5_1$byear >=1992 & Polity5_1$polity >=-10)
Belarus_polity
Belarus_polity_1992<-subset(Belarus_polity, Belarus_polity$country=="Belarus","polity") 
Belarus_polity_1992
summary(Belarus_polity_1992)
sd(Belarus_polity_1992$polity)
Modes <- function(Belarus_polity_1992) {      
  unique_values <- unique(Belarus_polity_1992)
  tabulated_values <- tabulate(match(Belarus_polity_1992, unique_values))
  all_modes <- unique_values[tabulated_values == max(tabulated_values)]
  return(all_modes)
}
Modes(Belarus_polity_1992$polity)
#Альтернативный способ расчета значений для каждой страны № 2 ====
tapply(Polity5_1$polity, Polity5_1$country=="Russia"& Polity5_1$byear >=1992& Polity5_1$polity >=-10, summary, na.rm = T)
tapply(Polity5_1$polity, Polity5_1$country=="Ukraine"& Polity5_1$byear >=1992& Polity5_1$polity >=-10, summary, na.rm = T)
tapply(Polity5_1$polity, Polity5_1$country=="Belarus"& Polity5_1$byear >=1992& Polity5_1$polity >=-10, summary, na.rm = T)
tapply(Polity5_1$polity, Polity5_1$country=="Russia"& Polity5_1$byear >=1992& Polity5_1$polity >=-10, sd, na.rm = T)
tapply(Polity5_1$polity, Polity5_1$country=="Ukraine"& Polity5_1$byear >=1992& Polity5_1$polity >=-10, sd, na.rm = T)
tapply(Polity5_1$polity, Polity5_1$country=="Belarus"& Polity5_1$byear >=1992& Polity5_1$polity >=-10, sd, na.rm = T)
#Таблица: Украина, Россия, Беларусь с 1992 года ====
Bratya_slavyane<-subset(Polity5_1, (Polity5_1$country=="Russia"|Polity5_1$country=="Belarus"|Polity5_1$country=="Ukraine") & Polity5_1$byear>=1992)
Bratya_slavyane
#ВЫВОДЫ ==== 
#Если говорить о максимальных значениях индекса Polity, то здесь между тремя рассматриваемыми случаями наблюдаются минимальные различия. Режим в Украине и Беларуси характеризовался как демократический в первой половине девяностых годов: во время транзитного периода от членства в Советском Союзе до формирования институтов суверенного государства (7 и 8 баллов соответственно). При этом показатели Беларуси стремительно упали в 1995 году, когда в стране были проведены парламентские выборы, охаракетризованные многими международными наблюдателями как не свободные. Отметим, что в Украине существует и второй пик "максимальной демократизации", пришедшийся на период с 2006 по 2010 год. Судя по всему, это связано с победой "Оранжевой революции" и президентством Виктора Ющенко, провозглазившего курс на широкую интеграцию в европейские институты и развитие гражданских прав и свобод. 
#В случае России наблюдается "отложенный" выход на пик демократизации, наблюдавшийся  в период с 2000 по 2007 годы (6 баллов). Для российского случая 90-е годы характеризовались постоянными конфликтами между различными группами элит, которые приводили к нарушению баланса сдержек и противовесов (август 1993 года), а в отдельных случаях - манипуляциями с чистотой политической конкуренции и попытками внеинституциональной дискредитации оппонентов ("Голосуй или проиграешь!", 1996). Максимальное значение индекса Polity в России (2000 - 2007) было достигнуто во время процесса формирования "вертикали власти", когда в стране еще существовала активная политическая конкуренция, но федеральный центр уже устанавливал общие правила игры и вводил огранчиения на прямое использование силы и манипуляций в политике для внутриэлитных групп, добиваясь постепенной монополии на это право. 
#Рассматривая минимальные значения, следует отметить, что показатель по Беларуси (-7) разительно отличается от аналогичного для России (3) и Украины(4). Действительно, режим Александра Лукашенко значительно более жестко ограничивает политическую конкуренцию и участие, а власть фактически является несменяемой и ротируется только по решению самого автократа. 
#Это же подтверждается и показателями "медиана" и "среднее арифметическое". Медиана для Беларуси - 0,00, для России - 4,5, для Украины - 6,0. Примерно в тех же значениях лежат и средние арифметические. Согласно классификации Polity, эти усредненные показатели позволяют Украине оставаться на нижней границе группы "демократических" стран, России - входить в группу "открытых анократий", а Беларуси - в группу "Закрытых". При этом интересно, что последние наблюдения (до 2018 года) ставят Россию и Украину в один ряд, присуждая им по 4 балла. Хотя другие источники свидетельствуют о формировании в Украине гораздо более конкурентной политической среды и более стабильной сменяемости власти. Судя по всему, балл Украины оказался занижен из-за неспособности контролировать часть территории страны, фактическиого запрета на работу отдельных политических сил (Коммунистическая партия) и общей хрупкости институтов. Балл России, в свою очередь, мог быть завышен из-за включения в последний показатель наблюдений с 2007 по 2011 годы. Можно предположить, что в новой базе Polity балл Украины будет возрастать, а балл России из-за усилившихся преследований по политическим мотивам и жесткого подавления протестов, напротив, может быть понижен.
#Показатель "стандартного отклонения" по сути фиксирует стремительность деградации белорусских институтов  демократии в середине 90-ых годов, а также может говорить о сравнительно небольших отклонениях показателей для Украины и России от общего для этих стран тренда. 
#ВОПРОС № 1. БУДУ БЛАГОДАРЕН, ЕСЛИ СМОЖЕТЕ ПОДСКАЗАТЬ. ОЧЕВИДНО, ЧТО МОЖНО В ОДНОМ АРГУМЕНТЕ (??) ОТСЫЛАТЬСЯ СРАЗУ НА СЛУЧАИ РОССИИ, УКРАИНЫ, БЕЛАРУСИ. НО ЕСЛИ Я ЗАПУСКАЮ ЭТОТ КОД, ТО ПРОГРАММА ВЫДАЕТ ВСЕ СТОЛБЦЫ И ТОЛЬКО 4 СТРОКИ ИЗ 15.====
#ПРЕДПОЛАГАЮ, ЧТО ОШИБКА ГДЕ-ТО ЗДЕСЬ: c("Russia","Belarus","Ukraine"). НО В ЧЕМ ИМЕННО?
#Bratstvo_narodov<-subset(Polity5_1, Polity5_1$country==c("Russia","Belarus","Ukraine") & Polity5_1$byear>=1992)
#Bratstvo_narodov 
#ВОПРОС № 2. Почему я не могу посчитать моду отдельнодля каждой страны, если перед этим не извлекаю вектор из общей таблицы? Например: ====
#Modes <- function(Bratya_slavyaneNumbers) {      
 # unique_values <- unique(Bratya_slavyaneNumbers)
 # tabulated_values <- tabulate(match(Bratya_slavyaneNumbers, unique_values))
 # all_modes <- unique_values[tabulated_values == max(tabulated_values)]
 # return(all_modes)
#}
#Modes(Bratya_slavyaneNumbers$polity,Bratya_slavyaneNumbers$country)
