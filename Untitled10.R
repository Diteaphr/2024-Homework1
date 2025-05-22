{
  "nbformat": 4,
  "nbformat_minor": 0,
  "metadata": {
    "colab": {
      "provenance": [],
      "authorship_tag": "ABX9TyMkOBHAQMIyBYsktHKySEPY",
      "include_colab_link": true
    },
    "kernelspec": {
      "name": "ir",
      "display_name": "R"
    },
    "language_info": {
      "name": "R"
    }
  },
  "cells": [
    {
      "cell_type": "markdown",
      "metadata": {
        "id": "view-in-github",
        "colab_type": "text"
      },
      "source": [
        "<a href=\"https://colab.research.google.com/github/Diteaphr/2024-Homework1/blob/main/Untitled10.R\" target=\"_parent\"><img src=\"https://colab.research.google.com/assets/colab-badge.svg\" alt=\"Open In Colab\"/></a>"
      ]
    },
    {
      "cell_type": "code",
      "source": [
        "install.packages(\"titanic\")\n",
        "\n",
        "# Load Titanic dataset\n",
        "library(titanic)\n",
        "data(\"titanic_train\")\n",
        "str(titanic_train)\n"
      ],
      "metadata": {
        "colab": {
          "base_uri": "https://localhost:8080/"
        },
        "id": "OqzZ_iNX0uUw",
        "outputId": "db2a8a99-99be-4a74-b6e8-9d9aaf6870c1"
      },
      "execution_count": 9,
      "outputs": [
        {
          "output_type": "stream",
          "name": "stderr",
          "text": [
            "Installing package into ‘/usr/local/lib/R/site-library’\n",
            "(as ‘lib’ is unspecified)\n",
            "\n"
          ]
        },
        {
          "output_type": "stream",
          "name": "stdout",
          "text": [
            "'data.frame':\t891 obs. of  12 variables:\n",
            " $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...\n",
            " $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...\n",
            " $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...\n",
            " $ Name       : chr  \"Braund, Mr. Owen Harris\" \"Cumings, Mrs. John Bradley (Florence Briggs Thayer)\" \"Heikkinen, Miss. Laina\" \"Futrelle, Mrs. Jacques Heath (Lily May Peel)\" ...\n",
            " $ Sex        : chr  \"male\" \"female\" \"female\" \"female\" ...\n",
            " $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...\n",
            " $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...\n",
            " $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...\n",
            " $ Ticket     : chr  \"A/5 21171\" \"PC 17599\" \"STON/O2. 3101282\" \"113803\" ...\n",
            " $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...\n",
            " $ Cabin      : chr  \"\" \"C85\" \"\" \"C123\" ...\n",
            " $ Embarked   : chr  \"S\" \"C\" \"S\" \"S\" ...\n"
          ]
        }
      ]
    },
    {
      "cell_type": "code",
      "source": [
        "# Load necessary libraries\n",
        "library(titanic)\n",
        "library(dplyr)\n",
        "\n",
        "# Load the data\n",
        "data(\"titanic_train\")\n",
        "\n",
        "# Prepare the dataset: select Survived, SibSp, and Parch, and remove missing values\n",
        "titanic_data <- titanic_train %>%\n",
        "  select(Survived, SibSp, Parch) %>%\n",
        "  na.omit()  # Not strictly necessary here as these columns have no NAs\n",
        "\n",
        "# Fit logistic regression model\n",
        "model_family <- glm(Survived ~ SibSp + Parch, data = titanic_data, family = \"binomial\")\n",
        "\n",
        "# Show model summary\n",
        "summary(model_family)\n",
        "\n",
        "# Predict survival probability\n",
        "pred_prob <- predict(model_family, type = \"response\")\n",
        "\n",
        "# Convert probabilities to binary predictions (0/1)\n",
        "pred_class <- ifelse(pred_prob > 0.5, 1, 0)\n",
        "\n",
        "# Calculate accuracy\n",
        "accuracy <- mean(pred_class == titanic_data$Survived)\n",
        "print(paste(\"Prediction Accuracy:\", round(accuracy, 4)))\n",
        "\n",
        "# Load libraries\n",
        "library(ggplot2)\n",
        "library(dplyr)\n",
        "\n",
        "# Create a grid of all combinations of SibSp and Parch\n",
        "new_data <- expand.grid(SibSp = 0:8, Parch = 0:6)\n",
        "\n",
        "# Predict survival probabilities on the grid\n",
        "new_data$Survival_Prob <- predict(model_family, newdata = new_data, type = \"response\")\n",
        "\n",
        "# Plot heatmap\n",
        "ggplot(new_data, aes(x = SibSp, y = Parch, fill = Survival_Prob)) +\n",
        "  geom_tile() +\n",
        "  scale_fill_gradient(low = \"blue\", high = \"red\") +\n",
        "  labs(title = \"Predicted Survival Probability\",\n",
        "       x = \"Number of Siblings/Spouses (SibSp)\",\n",
        "       y = \"Number of Parents/Children (Parch)\",\n",
        "       fill = \"Survival Probability\") +\n",
        "  theme_minimal()\n",
        "\n"
      ],
      "metadata": {
        "colab": {
          "base_uri": "https://localhost:8080/",
          "height": 801
        },
        "id": "xrug67Pn18Tc",
        "outputId": "24094ce1-8d03-4e86-ad37-f87d27630fd5"
      },
      "execution_count": 8,
      "outputs": [
        {
          "output_type": "display_data",
          "data": {
            "text/plain": [
              "\n",
              "Call:\n",
              "glm(formula = Survived ~ SibSp + Parch, family = \"binomial\", \n",
              "    data = titanic_data)\n",
              "\n",
              "Coefficients:\n",
              "            Estimate Std. Error z value Pr(>|z|)    \n",
              "(Intercept) -0.50518    0.07979  -6.331 2.43e-10 ***\n",
              "SibSp       -0.16033    0.07312  -2.193  0.02832 *  \n",
              "Parch        0.29325    0.09545   3.072  0.00212 ** \n",
              "---\n",
              "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n",
              "\n",
              "(Dispersion parameter for binomial family taken to be 1)\n",
              "\n",
              "    Null deviance: 1186.7  on 890  degrees of freedom\n",
              "Residual deviance: 1175.7  on 888  degrees of freedom\n",
              "AIC: 1181.7\n",
              "\n",
              "Number of Fisher Scoring iterations: 4\n"
            ]
          },
          "metadata": {}
        },
        {
          "output_type": "stream",
          "name": "stdout",
          "text": [
            "[1] \"Prediction Accuracy: 0.6229\"\n"
          ]
        },
        {
          "output_type": "display_data",
          "data": {
            "text/plain": [
              "plot without title"
            ],
            "image/png": "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\nSURBVOxsmQNlZ2cPGzbMGiUBAACgJMwFu5SUlPDw8MOHDxc7yuHDh8PDww8cOGC9wgAAAGAZ\nc8HuxIkTlStXjoyMbN269erVq2/cuFGow40bN1avXt26devIyMjKlSufOHFCyVIBAABgjrlr\n7CpWrLhnz56vv/76vffeGzp0qBAiICDAz8/P29v7/v376enpf/75pxCidu3a69ati42NZRc6\nAACAMqSRc6eHgoKCI0eO7N+//9SpU3fu3Hnw4IGXl5e/v3+jRo3at28fERFRbjdjvKdpU9Yl\nWCxP2N52MLm2VrPNFSyEyKFm5T0S3mVdgsUe2dpBFkJkCc/iO5UzvQ0xMntqtVpfX19FiwHM\nkHXnCXt7+9atW7du3VrpagAAAFBinDwFAABQCYIdAAAqkZ6ePnfu3MaNG/v5+Tk6OlaqVKlz\n58779u1T4r3Cw8NDQkKecpCIiIgnDfLuu+9q/peXl1fr1q0TExOVeLunfK3p0TDtaZWjZBFZ\np2IBAEA5p9VqmzZtmpaWNnTo0IkTJ9rb2//2229xcXHR0dHr16/v27evdd+ub9++8je7LbEZ\nM2bUrFlTCKHX61NTU7/88svevXsvWrRo3LhxSr+1RZ50NEzbT58+3ahRIzlrG54GwQ4AADVY\nu3bt1atX4+PjX331VWPjm2++Wa9evenTp/fp08e6m1eMHz/eiqM9Sffu3cPDw40Pp06dWq9e\nvbfffnvEiBEuLi6lUIBMTzoapu3fffddKVTCqVgAANTg1q1bQojGjRubNvr4+Bw7duz8+fNS\nqmvYsGHDhg1NO/To0cPPz0/6PSIiolWrVjt37gwODm7evHlERISfn19+fr5p//Dw8MDAwIKC\nAuNJRvPdhBDx8fEvvviim5ubl5dXkyZN4uPjS/wZPT09e/fu/fDhwzNnzhQtWOqzZ8+eVq1a\neXp6urq61q1b9+OPPzadJNNoNCdPnmzZsqW7u7uvr++gQYPu3btnfNZ8qWZe+6RTrsb2zp07\njx07VhqkSZMm0kHLy8sz7RwZGenv76/T6Up8fATBDgAAdQgLCxNCTJ061TSpCCGCgoJcXV3l\njODs7Hz//v0pU6bMmDFj5syZsbGxd+/ePXjwoLHDtWvXfvzxx379+pluc2a+28aNG/v16xcU\nFLR58+YNGzb4+/v369dv165dJf6Ybm5uQggp/RQqWAiRlJTUpUsXd3f3r776aufOnZ06dZo0\n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"
          },
          "metadata": {
            "image/png": {
              "width": 420,
              "height": 420
            }
          }
        }
      ]
    },
    {
      "cell_type": "markdown",
      "source": [
        "## Logistic Regression Analysis: Survival ~ SibSp + Parch\n",
        "\n",
        "### 1. Model Specification\n",
        "\n",
        "We fit a logistic regression model to predict the probability of survival (`Survived`) on the Titanic using two predictors:\n",
        "- `SibSp`: Number of siblings/spouses aboard\n",
        "- `Parch`: Number of parents/children aboard\n",
        "\n",
        "```r\n",
        "model_family <- glm(Survived ~ SibSp + Parch, data = titanic_data, family = \"binomial\")\n",
        "summary(model_family)\n",
        "```\n",
        "\n",
        "### 2. Model Output\n",
        "\n",
        "```\n",
        "Call:\n",
        "glm(formula = Survived ~ SibSp + Parch, family = \"binomial\", data = titanic_data)\n",
        "\n",
        "Coefficients:\n",
        "             Estimate Std. Error z value Pr(>|z|)    \n",
        "(Intercept)  -0.50518    0.07979  -6.331 2.43e-10 ***\n",
        "SibSp        -0.16033    0.07312  -2.193   0.02832 *  \n",
        "Parch         0.29325    0.09545   3.072   0.00212 **\n",
        "---\n",
        "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n",
        "\n",
        "Residual deviance: 1175.7  on 888 degrees of freedom\n",
        "AIC: 1181.7\n",
        "```\n",
        "\n",
        "### 3. Interpretation of Coefficients\n",
        "\n",
        "- **Intercept**: When both `SibSp` and `Parch` are 0, the log-odds of survival is -0.50518.\n",
        "- **SibSp**: Each additional sibling/spouse aboard decreases the log-odds of survival by 0.16033. The odds ratio is approximately exp(-0.16033) ≈ 0.852, indicating a ~15% decrease in odds of survival per additional sibling/spouse.\n",
        "- **Parch**: Each additional parent/child aboard increases the log-odds of survival by 0.29325. The odds ratio is approximately exp(0.29325) ≈ 1.341, indicating a ~34% increase in odds of survival per additional parent/child.\n",
        "\n",
        "Both predictors are statistically significant at conventional levels.\n",
        "\n",
        "### 4. Model Accuracy\n",
        "\n",
        "Prediction accuracy using a 0.5 threshold:\n",
        "\n",
        "```r\n",
        "pred_prob <- predict(model_family, type = \"response\")\n",
        "pred_class <- ifelse(pred_prob > 0.5, 1, 0)\n",
        "mean(pred_class == titanic_data$Survived)\n",
        "```\n",
        "\n",
        "```\n",
        "[1] \"Prediction Accuracy: 0.6229\"\n",
        "```\n",
        "\n",
        "The model achieves an accuracy of approximately 62.3%, suggesting that family composition has some predictive power for survival, though other variables may contribute more strongly.\n"
      ],
      "metadata": {
        "id": "h5VrxJto3MCd"
      }
    }
  ]
}