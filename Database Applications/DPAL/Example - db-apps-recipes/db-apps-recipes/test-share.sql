-- Register 2 users and fill their lists with different items

-- Fill Honza's shopping list

execute Register 'Honza', 'Novak', 'honza.novak@gmail.com'
declare @HonzaShoppingListId int
select @HonzaShoppingListId = l.Id from ShoppingList l inner join Account a ON a.Id = l.OwnerId where a.Email = 'honza.novak@gmail.com'
declare @HonzaId int
select @HonzaId = Id from Account where Email = 'honza.novak@gmail.com'

execute AddFoodstuffToShoppingList @HonzaShoppingListId, 'Onion'
execute AddFoodstuffToShoppingList @HonzaShoppingListId, 'Carrot'

declare @OmelleteId int
select @OmelleteId = Id from Recipe where [Name] = 'Cheese omellete'
execute AddRecipeToShoppingList @HonzaShoppingListId, @OmelleteId

-- Fill Lukas's shopping list

execute Register 'Lukas', 'Novak', 'lukas.novak@gmail.com'
declare @LukasShoppingListId int
select @LukasShoppingListId = l.Id from ShoppingList l inner join Account a ON a.Id = l.OwnerId where a.Email = 'lukas.novak@gmail.com'
declare @LukasId int
select @LukasId = Id from Account where Email = 'lukas.novak@gmail.com'

execute AddFoodstuffToShoppingList @LukasShoppingListId, 'Tomato'
execute AddFoodstuffToShoppingList @LukasShoppingListId, 'Spring onion'

declare @SauceId int
select @SauceId = Id from Recipe where [Name] = 'Cheese Bechamel sauce'
execute AddRecipeToShoppingList @LukasShoppingListId, @SauceId

-- See what they have in ALL shopping lists

select * from FoodstuffInAllShoppingLists
select * from RecipesInAllShoppingLists

-- Share shopping lists

execute Share @HonzaShoppingListId, @LukasId
execute Share @LukasShoppingListId, @HonzaId

-- See all shared shopping lists

select * from SharedShoppingLists

-- Show items in all shopping lists

select * from FoodstuffInAllShoppingLists
select * from RecipesInAllShoppingLists

-- Cannot share already shared shopping list

begin try
	execute Share @HonzaShoppingListId, @LukasId
print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch

-- Cannot share shopping list with the owner

begin try
	execute Share @LukasShoppingListId, @LukasId
print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch
