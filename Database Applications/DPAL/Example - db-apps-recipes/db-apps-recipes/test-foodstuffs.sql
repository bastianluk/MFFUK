-- Foodstuff tests

-- Create new account for testing i

execute Register 'Petr', 'Novak', 'petr.novak@gmail.com'

declare @ShoppingListId int
select @ShoppingListId = l.Id from ShoppingList l inner join Account a ON a.Id = l.OwnerId where a.Email = 'petr.novak@gmail.com'

-- See what is in my shopping lists

select * from FoodstuffInOwnedShoppingLists

-- See all foodstuff, try to search for bread (is not in database)

select * from FoodstuffIndex
select * from SearchFoodstuffs('Bread')

-- Add Bread to shopping list

execute AddFoodstuffToShoppingList @ShoppingListId, 'Bread'

-- See that bread is in shopping list

select * from FoodstuffInOwnedShoppingLists

-- See that bread is added to foodstuffs in database

select * from FoodstuffIndex
select * from SearchFoodstuffs('Bread')

-- Try to add foodstuff already present in the shopping list and fail

begin try
	execute AddFoodstuffToShoppingList @ShoppingListId, 'Bread'
print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch

-- Remove bread from shopping list

execute RemoveFoodstuffToShoppingList @ShoppingListId, 'Bread'

-- Remove non existing foodstuff 

begin try
	execute RemoveFoodstuffToShoppingList @ShoppingListId, 'Non existing foodstuff'
print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch

-- See that bread is no more in shopping list

select * from FoodstuffInOwnedShoppingLists

-- See that bread is still in foodstuffs in database

select * from FoodstuffIndex
select * from SearchFoodstuffs('Bread')
