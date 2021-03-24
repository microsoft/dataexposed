# Using R for Logistic Regression. Code from all over the place - this is a pretty common demo. Similarities are unintentional. 
# We'll use a newer version of the Pima Indian dataset to predict diabetes. More on the original dataset here: https://www.kaggle.com/uciml/pima-indians-diabetes-database 

# You'll need to install an few libraries if you don't already have them
install.packages("tidyverse")
install.packages("caret")
install.packages("mlbench")

# Now load the libraries you need, and set some plot elements
library(tidyverse)
library(caret)

# Load the data and do a little feature engineering. We're using a newer version of the Pima data
# from mlbench since the original Pima dataset had some issues. 
data("PimaIndiansDiabetes2", package = "mlbench")
PID2 <- na.omit(PimaIndiansDiabetes2)

# Not much EDA this time - we'll just view the data
sample_n(PID2, 4)

# Now we split the data into training and test set
set.seed(123)
training.samples <- PID2$diabetes %>% 
createDataPartition(p = 0.75, list = FALSE)
train.data  <- PID2[training.samples, ]
test.data <- PID2[-training.samples, ]

# With that done, we can fit this to the model
model <- glm( diabetes ~., data = train.data, family = binomial)

# Let's take a look at that model
summary(model)

# Now we can Make predictions with it
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

# And we can check the model's accuracy
mean(predicted.classes == test.data$diabetes)
model <- glm( diabetes ~ glucose, data = train.data, family = binomial)
summary(model)$coef
newdata <- data.frame(glucose = c(20,  180))
probabilities <- model %>% predict(newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
predicted.classes

# Let's start to visualize. We'll set some plot elements
theme_set(theme_linedraw())
theme_update(panel.grid.minor = element_line(colour = "red"))

# We can now create the plot
train.data %>%
  mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
  ggplot(aes(glucose, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Plasma/Glucose Concentration",
    y = "Prob. Diabetes +"
    )


# If you want, you can take this further and show Multiple Logistic Regression
model <- glm( diabetes ~ glucose + mass + pregnant, 
                data = train.data, family = binomial)
summary(model)$coef

model <- glm( diabetes ~., data = train.data, family = binomial)
summary(model)$coef

