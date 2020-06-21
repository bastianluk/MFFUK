execute Register 'Eduard', 'Novak', 'eduard.novak@gmail.com'

declare @ShoppingListId int
select @ShoppingListId = l.Id from ShoppingList l inner join Account a ON a.Id = l.OwnerId where a.Email = 'eduard.novak@gmail.com'

-- See recipes that can be cooked

select * from RecipesThatCanBeCooked

-- Add some foodstuff into shopping list

execute AddFoodstuffToShoppingList @ShoppingListId, 'Egg'
execute AddFoodstuffToShoppingList @ShoppingListId, 'Cheddar cheese'

-- Add recipe

declare @OmelleteId int
select @OmelleteId = Id from Recipe where [Name] = 'Cheese omellete'
execute AddRecipeToShoppingList @ShoppingListId, @OmelleteId

-- See shopping list

select * from FoodstuffInOwnedShoppingLists
select * from RecipesInOwnedShoppingLists

-- See now recipes that can be cooked

select * from RecipesThatCanBeCooked

-- Cook recipe when there is enough resources.

execute Cook @ShoppingListId, @OmelleteId

-- Check the state after cooking

select * from FoodstuffInAllShoppingLists
select * from RecipesInAllShoppingLists
select * from RecipesThatCanBeCooked

-- Fail to cook non existing recipe in shopping list

begin try
	execute Cook @ShoppingListId, @OmelleteId
print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch

-- Fail to cook recipe with not enough resources 

execute AddRecipeToShoppingList @ShoppingListId, @OmelleteId

begin try
	execute Cook @ShoppingListId, @OmelleteId
print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch

