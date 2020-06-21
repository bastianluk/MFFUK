-- Foreign key indexes
CREATE INDEX FoodstuffShoppingListItem_ShoppingListId ON FoodstuffShoppingListItem(ShoppingListId);
CREATE INDEX FoodstuffShoppingListItem_FoodstsuffId ON FoodstuffShoppingListItem(FoodstuffId);
CREATE INDEX Ingredient_RecipeId ON Ingredient(RecipeId);
CREATE INDEX Ingredient_FoodstuffId ON Ingredient(FoodstuffId);
CREATE INDEX RecipeShoppingListItem_ShoppingListId ON RecipeShoppingListItem(ShoppingListId);
CREATE INDEX RecipeShoppingListItem_RecipeId ON RecipeShoppingListItem(RecipeId);
CREATE INDEX SharedShoppingList_ShoppingListId ON SharedShoppingList(ShoppingListId);
CREATE INDEX SharedShoppingList_AccountId ON SharedShoppingList(AccountId);
CREATE INDEX ShoppingList_OwnerId ON ShoppingList(OwnerId);

-- Indexes for searching by name
CREATE INDEX Foodstuff_Name ON Foodstuff([Name]);
CREATE INDEX Recipe_Name ON Recipe([Name]);

-- Index for searching user by email
CREATE INDEX Account_Email ON Account([Email]);