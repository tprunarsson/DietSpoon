# A modern look at Stigler's diet problem
# Solution template

# The set of dishes
set dish;
param dishname symbolic;

# The set of ingredience
set ingredient;

# The set of nutrients
set nutrient;

# Indicator to tell us that the dish is chosen
var x{dish} binary;

# Indicator to tell us that an ingredient is chosen
var y{ingredient} binary;

# A popularity measure for the dish
param aggregateLikes{dish};

# The price of a dish
param pricePerServing{dish};

# The time taken to make the dish
param readyInMinutes{dish};

# The amount if nutrients found in a dish
param dishnutrient{dish,nutrient} >= 0, default 0;
param daily_req{nutrient} default 0;

# The ingredients per dish and the number of them 
param dishingredient{dish,ingredient} >= 0, default 0;
param numberofingredient{d in dish} := sum{i in ingredient: dishingredient[d,i] > 0} 1;

# Add your constraints here:

# For example: select 5 dishes, plan for a week
subject to theplan: sum{d in dish} x[d] = 5;

# Add missing constraints here below...

# The objective function, note that each objective should have a weight
# For example: here only two goals is given, the popularity and number of ingredients
minimize objfunction:
  - sum{d in dish} aggregateLikes[d] * x[d]
  + sum{i in ingredient} y[i];
