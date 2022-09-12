# CT5102 - Week 1 Assignment
# Author: Niall Guerin
# Student ID: 18235079

# Task 1
# Set the seed per specification and allow for 2 dice
set.seed(99); 
dice_rolls <- sample(1:6, 1000, replace = TRUE, prob = NULL ) + sample(1:6, 1000, replace = TRUE, prob = NULL )
outcomes <- ifelse(dice_rolls %% 2 == 0,"Even", "Odd")
display_results <- c("Odd Numbers" = sum(outcomes == "Odd", na.rm = FALSE), "Even Numbers" = sum(outcomes == "Even", na.rm = FALSE))
print(display_results)
cat("\n")

# Task 2:
# Create a vector to contain first AND second dice value ranges in dice_rolls (i.e. we never have as a result value in the sum, because we have 2 dice so always 2-12 is our possible values)
# You can use this as the label vector in the result.
dice_roll_sum_values <- c(2:12)

# uses a loop to iterate over [i] and allow for easier writing of target values
# Initializing total_dice_outcomes_freq with sum of dice_rolls based on first
# element of x and loop from there to avoid having to format loop later. 
total_dice_outcomes_freq <- sum(dice_rolls == dice_roll_sum_values[1])

for(i in 2:length(dice_roll_sum_values) )
{
  dice_rolls_sum <- (sum(dice_rolls == dice_roll_sum_values[i]))
  total_dice_outcomes_freq <- c(total_dice_outcomes_freq, dice_rolls_sum)
}

# We still need the labels as numbers to give the complete vector requested in step 2, not just a results values vector. Refer to slide 21 of week 1 deck. Vector sizes are equal.
names(total_dice_outcomes_freq) <- dice_roll_sum_values
print(total_dice_outcomes_freq)
cat("\n")

# Get the first element from dice_rolls vector and concatenate that result with the next 100th position and repeat i.e. 1-99 = F, 100th position = T on repeat
diceRollsCombined100thElement <- c(dice_rolls[1], dice_rolls[ c( rep(FALSE, 99), TRUE ) ])
print(diceRollsCombined100thElement)

# Below are just test cases for validation of the last step as expected result is not listed in this step so writing test cases to directly confirm expected values.
# Validate the Test Result Data: Print off first element, then print off 100th element and each successive 100th dice roll element in dice_rolls vector)
print(dice_rolls[1])
count <- 100
while(count <= 1000){
 print(dice_rolls[count])
 count <- count + 100
}

# If any issue with loop validation, just revert to these expicit print[element value] test cases for final validation. Uncomment if required.
# print(dice_rolls[1])
print(dice_rolls[100])
print(dice_rolls[200])
# print(dice_rolls[300])
# print(dice_rolls[400])
# print(dice_rolls[500])
# print(dice_rolls[600])
# print(dice_rolls[700])
# print(dice_rolls[800])
# print(dice_rolls[900])
# print(dice_rolls[1000])