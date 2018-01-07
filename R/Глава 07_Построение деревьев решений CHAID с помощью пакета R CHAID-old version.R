
# 7.1.1 Глава 7 Построение деревьев решений CHAID с помощью пакета R CHAID

# 7.1 Построение и интерпретация дерева классификации CHAID
# 7.1.1 Подготовка данных

# Задаем постоянный CRAN репозиторий
cat(".Rprofile: Setting US repositoryn")
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
rm(r)

# Устанавливаем необходимые для работы пакеты
install.packages("CHAID", repos="http://R-Forge.R-project.org")
install.packages("pROC")
install.packages("rattle")
install.packages("smbinning")
# Считываем дерево
data <- read.csv2("C:/Trees/Churn_binned.csv")
str(data)
# Переводим количественную переменную в порядковую
data$longdist2 <- ordered(data$longdist, levels = c("<2", "2--8", " 9--14", "15-20", "21+"))
data$longdist <- NULL

data$local2 <- ordered(data$local, levels = c("<8", "8--20", "21-35", "36-55", "56+"))
data$local <- NULL

data$incomecat2 <- ordered(data$incomecat, levels = c("<15", "15-35", "36-50", "51-65", "66+"))
data$incomecat <- NULL

data$agecat2 <- ordered(data$agecat, levels = c("<31", "31-45", "46-58", "59-70", "71+"))
data$agecat <- NULL

str(data)

# 7.1.2 Построение модели и работа с диаграммой дерева
# функция set.seed() (от set - задать, установить, и seed - начальное число). Как следует из названия, эта функция фиксирует число, служащее начальной точкой для запуска алгоритма генерации (псевдо-)случайных чисел. В качестве аргумента функции указывают любое целое число (не важно, какое именно). Так, при повторном выполнении следующего кода мы будем всегда получать одинаковые последовательности значений
set.seed(42)
library(CHAID)
# Создаем дерево chaid. chaid_control -функция(minprob=минимальная относительная частота наблюдений в терминальном узле,
# minbucket=минимальное абсолютное количество наблюдений в терминальном узле,
# minsplit=минимальное число наблюдений в узле перед расщеплением)
model.chaid  <- chaid(churn ~ ., 
                      control = chaid_control
                      (minprob = 0.001,
                        minsplit = 350,minbucket = 100),
                      data)

plot(model.chaid)
print(model.chaid)

# 7.1.3 Вычисление вероятностей классов и выбор оптимального порога
# Predict по каждому клиенту делаем прогноз того останется клиент или уйдет
predict(model.chaid, data, type="prob")
predict(model.chaid, data, type="prob")[4427:4431]
score <- predict(model.chaid, data, type="prob")

library(pROC)
roc <- roc(data$churn, score[,1], ci=TRUE)
plot(roc)
roc

roc2 <- plot.roc(data$churn, score[,1],
                 main="ROC-кривая для модели дерева CHAID",  
                 percent=TRUE, ci=TRUE, # вычислить AUC
                 print.auc=TRUE) # напечатать значение AUC (вместе с довер. интервалом)

ci.thresholds(roc)
coords(roc, # ROC-кривая модели 
       "best", # вид критерия 
       best.method="youden", # способ расчета критерия
       ret=c("threshold", "spec", "sens")) # какие параметры возвращать

data.t <- table(data$churn)
w <- as.numeric(data.t[2]/(data.t[1]+data.t[2]))

coords(roc, # ROC-кривая модели. Оптимальный критерий Йодена с поправкой на реальное количество ушедших клиентов
       "best", # вид критерия
       best.method="youden", # способ расчета критерия
       best.weights=c(1, w), # подставляем вычисленную долю ушедших в выборке
       ret=c("threshold", "spec", "sens")) # какие параметры возвращать
#Оптимальный порог согласно критерию Йодена, стоимость неправильной
#классификации ушедшего клиента в 2 раза больше стоимости неправильной
#классификации оставшегося клиента
coords(roc,"best", best.method="youden",
       best.weights=c(2, 0.5), # первый параметр увеличен вдвое
       ret=c("threshold", "spec", "sens"))

# 7.1.4 Получение спрогнозированных классов зависимой переменной

predict(model.chaid, data, type="response")[4427:4431]
result <- predict(model.chaid, data, type="response")
table(data$churn, result)

# 7.1.5 Сохранение прогнозов в тот же csv-file Для дальнейшей работы, например на Python

churnprob<-data.frame(data, result=score)
write.csv(churnprob, "churnprob.csv")

# 7.1.6 Применение модели к новым данным
#ПРеобразование в упорядоченные факторы
churnprognoz <- read.csv2("C:/Trees/Churn_new.csv")
str(churnprognoz)
churnprognoz$longdist2 <- ordered(churnprognoz$longdist, levels = c("<2", "2--8", " 9--14", "15-20", "21+"))
churnprognoz$longdist <- NULL

churnprognoz$local2 <- ordered(churnprognoz$local, levels = c("<8", "8--20", "21-35", "36-55", "56+"))
churnprognoz$local <- NULL

churnprognoz$incomecat2 <- ordered(churnprognoz$incomecat, levels = c("<15", "15-35", "36-50", "51-65", "66+"))
churnprognoz$incomecat <- NULL

churnprognoz$agecat2 <- ordered(churnprognoz$agecat, levels = c("<31", "31-45", "46-58", "59-70", "71+"))
churnprognoz$agecat <- NULL

str(churnprognoz)
predict(model.chaid, churnprognoz, type="prob")
newscore <- predict(model.chaid, churnprognoz, type="prob")
newclients_churnprognoz<-data.frame(churnprognoz, result=newscore)
write.csv(newclients_churnprognoz,
          "newclients_churnprognoz.csv")

# 7.1.7 Проверка модели

# 7.1.7.1 Однократное случайное разбиение набора данных на обучающую и контрольную выборки

data <- read.csv2("C:/Trees/Churn_binned.csv")

data$longdist2 <- ordered(data$longdist, levels = c("<2", "2--8", " 9--14", "15-20", "21+"))
data$longdist <- NULL
data$local2 <- ordered(data$local, levels = c("<8", "8--20", "21-35", "36-55", "56+"))
data$local <- NULL
data$incomecat2 <- ordered(data$incomecat, levels = c("<15", "15-35", "36-50", "51-65", "66+"))
data$incomecat <- NULL
data$agecat2 <- ordered(data$agecat, levels = c("<31", "31-45", "46-58", "59-70", "71+"))
data$agecat <- NULL
#Задаем стартовое значение генератора случайных чисел.
set.seed(42)

data$random_number <- runif(nrow(data),0,1)
development <- data[ which(data$random_number > 0.3), ]
holdout <- data[ which(data$random_number <= 0.3), ]

development$random_number <- NULL
holdout$random_number <- NULL

library(CHAID)

chaid.model  <- chaid(churn ~ . , 
                      control = chaid_control
                      (minprob = 0.001,
                        minsplit = 500, minbucket = 250),
                      data=development)
#Прогнозируем с помощью построенной модели CHAID вероятности классов для
#наблюдений обучающей выборки и записываем эти спрогнозированные вероятности
#в объект score_dev.
score_dev <- predict(chaid.model, development, type = "prob")
score_hold <- predict(chaid.model, holdout, type = "prob")

library(pROC)

roc_dev <-plot(roc(development$churn, score_dev[,1], ci=TRUE), percent=TRUE, print.auc=TRUE, col="#1c61b6")
roc_hold <-plot(roc(holdout$churn, score_hold[,1], ci=TRUE), percent=TRUE, 
                print.auc=TRUE, col="#008600", print.auc.y= .4, add=TRUE)
# Создаем легенды к ROC-кривым.
legend("bottomright", legend=c("Обучающая выборка", "Контрольная выборка"), 
       col=c("#1c61b6", "#008600"), lwd=2)

result_dev <- predict(chaid.model, development, type="response")
table(development$churn, result_dev)

result_hold <- predict(chaid.model, holdout, type="response")
table(holdout$churn, result_hold)

# 7.1.7.2 Многократное случайное разбиение набора данных на обучающую и контрольную выборки

data <- read.csv2("C:/Trees/Churn_binned.csv")
data$longdist2 <- ordered(data$longdist, levels = c("<2", "2--8", " 9--14", "15-20", "21+"))
data$longdist <- NULL
data$local2 <- ordered(data$local, levels = c("<8", "8--20", "21-35", "36-55", "56+"))
data$local <- NULL
data$incomecat2 <- ordered(data$incomecat, levels = c("<15", "15-35", "36-50", "51-65", "66+"))
data$incomecat <- NULL
data$agecat2 <- ordered(data$agecat, levels = c("<31", "31-45", "46-58", "59-70", "71+"))
data$agecat <- NULL
library(CHAID)
library(pROC)
chaid.auc = NULL
set.seed(42)
for (i in 1:1000) {
  data$random_number <- runif(nrow(data),0,1)
  development <- data[ which(data$random_number > 0.3), ]
  holdout     <- data[ which(data$random_number <= 0.3), ]
  development$random_number <- NULL
  holdout$random_number <- NULL
  chaid.model  <- chaid(churn ~ . , 
                        control = chaid_control
                        (minprob = 0.001,
                          minsplit = 500, minbucket = 250),
                        data=development)
  chaid.score <- predict(chaid.model, holdout, type = "prob")
  chaid.roc <- roc(holdout$churn, chaid.score[,1])
  chaid.auc[i] <- chaid.roc$auc
}
#Теперь вычисляем среднее значение и доверительный интервал AUC по результа-
#там 1000 экспериментов.
mean(chaid.auc)
ci.auc(chaid.roc)

# 7.1.7.3 Перекрестная проверка

data <- read.csv2("C:/Trees/Churn_binned.csv")
data$longdist2 <- ordered(data$longdist, levels = c("<2", "2--8", " 9--14", "15-20", "21+"))
data$longdist <- NULL
data$local2 <- ordered(data$local, levels = c("<8", "8--20", "21-35", "36-55", "56+"))
data$local <- NULL
data$incomecat2 <- ordered(data$incomecat, levels = c("<15", "15-35", "36-50", "51-65", "66+"))
data$incomecat <- NULL
data$agecat2 <- ordered(data$agecat, levels = c("<31", "31-45", "46-58", "59-70", "71+"))
data$agecat <- NULL
library(CHAID)

set.seed(42)
#Создаем индекс для блоков перекрестной проверки с помощью функции cut.-это пока не ясно
ind = cut(1:nrow(data), breaks=10, labels=F)
#Затем используем цикл for, чтобы 10 раз на девяти блоках осуществить обучение
#модели, а на десятом проверить модель. В качестве метрики качества используем пра-
# вильность.
accuracies = c()
for (i in 1:10)  {
  fit = chaid(churn ~., control = chaid_control (minprob = 0.001, 
                                                 minsplit = 500, minbucket = 250), data[ind != i,])
  predictions = predict(fit, data[ind == i, ! names(data) %in% c("churn")])
  correct_count = sum(predictions == data[ind == i,c("churn")])
  accuracies = append(correct_count / nrow(data[ind == i,]), accuracies)
}
#Выводим 10 значений правильности
accuracies

mean(accuracies)

# 7.2 Биннинг переменных

# 7.2.1 Биннинг в пакете rattle

data <- read.csv2("C:/Trees/Churn_rattle.csv")
library(rattle)
str(data)
#Для выполнения биннинга воспользуйтесь функцией binning. Функция имеет об-
#щий вид:
#binning(x, bins=4, method=c("quantile", "wtd.quantile", "kmeans"),
#labels=NULL, ordered=TRUE, weights=NULL)
# x - Количественная переменная
# bins - Количество категорий для биннинга
# method - Задает метод биннинга
# labels Метки или названия категорий
# ordered -Параметр определяет, делать из переменной упорядоченный фактор или нет
# weights - Вектор весов для наблюдений (задают при биннинге с помощью взвешенных квантилей)
longdist2<- binning(data$longdist, bins=3,
                    method="quantile", labels=NULL, 
                    ordered=TRUE, weights=NULL)
age2<- binning(data$age, bins=5, 
               method="quantile", labels=NULL, 
               ordered=TRUE, weights=NULL)
#Записываем категоризированные переменные в таблицу данных.
data<-data.frame(data, longdist2)
data<-data.frame(data, age2)
#Удаляем исходные переменные.
data$longdist<- NULL
data$age<- NULL
#Проверяем, как выглядят наши данные.
#Видим что Longdist2 и age2 Упорядочились и разбились по категориям
str(data)

# 7.2.2 Биннинг в пакете smbinning
#Если вас интересует биннинг, оптимизированный по зависимой переменной, вос-
#пользуйтесь пакетом smbinning. Для биннинга используется метод деревьев. Разбие-
#  ния выполняются так, чтобы полученные категории максимизировали различия по
#зависимой переменной.
data <- read.csv2("C:/Trees/Churn_smbinning.csv")
library(smbinning)
str(data)
#Пакет smbinning разрабатывался для задач кредитного скоринга. В этом пакете на-
#блюдения со значением бинарной зависимой переменной, равным 0, соответствуют
#«плохим» заемщикам, а наблюдения со значением бинарной зависимой переменной,
#равным 1, соответствуют «хорошим» заемщикам.

#Визуальный анализ показывает, что зависимая переменная churn является цело-
#численным вектором (выделена желтым фоном).

#Выполним оптимальный биннинг переменной age и запишем результаты биннинга
#в объект result.
result=smbinning (df=data, y="churn", x="age")
#Теперь выведем результаты, просто введя result и нажав Enter. В ответ получаем довольно объемную сводку.
#WoE – вес категории.
#Пропущенные значения группируются в отдельную категорию. Каждая категория должна содержать не менее 5% наблюдений. Категории не должны содержать нулевого количества событий или несобытий.
#Несмотря на то, что абсолютное значение WoE важно, разница между WoE групп играет ключевую роль. Чем больше разница между последующими категориями, тем выше предсказа- тельная способность данной характеристики.
result
#Результаты биннинга переменной age, записанные в виде SQL-кода
smbinning.sql(result)
#Для проверки качества биннинга с помощью WoE строится график значений WoE.
#Подробно есть в книге Прогнозное моделирование в IBM SPSS stat, R и Python на стр 339-341
smbinning.plot(ivout=result, option="WoE", 
               sub="Переменная age")
result2=smbinning.sumiv(df=data, y="churn")
result2
smbinning.sumiv.plot(sumivt=result2, cex=0.9)

data=smbinning.gen(df=data,ivout=result,
                   chrname="agebin")
data$age<- NULL
str(data)
