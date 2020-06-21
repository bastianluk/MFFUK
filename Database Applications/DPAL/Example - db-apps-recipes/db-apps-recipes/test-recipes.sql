
execute Register 'Tomas', 'Novak', 'tomas.novak@gmail.com'

declare @ShoppingListId int
select @ShoppingListId = l.Id from ShoppingList l inner join Account a ON a.Id = l.OwnerId where a.Email = 'tomas.novak@gmail.com'

-- See all recipes in shopping list

select * from RecipesInOwnedShoppingLists

-- See all recipes, search recipes

select * from RecipeIndex
select * from SearchRecipes('Cheese')

-- Add recipes i

declare @OmelleteId int
select @OmelleteId = Id from Recipe where [Name] = 'Cheese omellete'

execute AddRecipeToShoppingList @ShoppingListId, @OmelleteId

-- See all recipes in shopping list

select * from RecipesInOwnedShoppingLists

-- Remove recipe from shopping list

execute RemoveRecipeFromShoppingList @ShoppingListId, @OmelleteId

-- See all recipes in shopping list

select * from RecipesInOwnedShoppingLists
