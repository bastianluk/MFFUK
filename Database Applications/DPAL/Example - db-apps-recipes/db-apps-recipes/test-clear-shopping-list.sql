-- Foodstuff tests

-- Create new account for testing i

execute Register 'Matej', 'Novak', 'matej.novak@gmail.com'

declare @ShoppingListId int
select @ShoppingListId = l.Id from ShoppingList l inner join Account a ON a.Id = l.OwnerId where a.Email = 'matej.novak@gmail.com'

-- See what is in my shopping lists

select * from FoodstuffInOwnedShoppingLists
select * from RecipesInOwnedShoppingLists

-- Fill the shopping list

execute AddFoodstuffToShoppingList @ShoppingListId, 'Bread'
execute AddFoodstuffToShoppingList @ShoppingListId, 'Milk'
declare @OmelleteId int
select @OmelleteId = Id from Recipe where [Name] = 'Cheese omellete'
execute AddRecipeToShoppingList @ShoppingListId, @OmelleteId

-- See what is in my shopping lists

select * from FoodstuffInOwnedShoppingLists
select * from RecipesInOwnedShoppingLists

-- Clear the shoping list

execute ClearShoppingList @ShoppingListId

-- See what is in my shopping lists

select * from FoodstuffInOwnedShoppingLists
select * from RecipesInOwnedShoppingLists

