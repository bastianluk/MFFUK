-- Account specific views, can be turned easily into function taking single account and filtering only that

-- Shows all accounts and foodstuff in shopping lists they owe.
create view FoodstuffInOwnedShoppingLists as
		select 
		a.Email as [Account],
		f.[Name] as FoodstuffName
	from Account a
	inner join ShoppingList sl on sl.OwnerId = a.Id
	inner join FoodstuffShoppingListItem fsh on fsh.ShoppingListId = sl.Id
	inner join Foodstuff f on fsh.FoodstuffId = f.Id
go

-- Shows all accounts and recipes in shopping lists they owe.
create view RecipesInOwnedShoppingLists as
	select 
		a.Email as [Account],
		r.[Name] as [RecipeName]
	from Account a
	inner join ShoppingList sl on sl.OwnerId = a.Id
	inner join RecipeShoppingListItem rsh on rsh.ShoppingListId = sl.Id
	inner join Recipe r on rsh.RecipeId = r.Id
go

-- Shows all accounts and foodstuff in all of their shopping lists.
create view FoodstuffInAllShoppingLists as
	select 
		a.Email as [Account],
		f.[Name] as FoodstuffName
	from Account a
	inner join SharedShoppingList sh on sh.AccountId = a.Id
	inner join FoodstuffShoppingListItem fsh on fsh.ShoppingListId = sh.ShoppingListId
	inner join Foodstuff f on fsh.FoodstuffId = f.Id
	union 
	select * from FoodstuffInOwnedShoppingLists
go

-- Shows all accounts and recipes in all of their shopping lists.
create view RecipesInAllShoppingLists as
	select 
		a.Email as [Account],
		r.[Name] as [RecipeName]
	from Account a
	inner join SharedShoppingList sh on sh.AccountId = a.Id
	inner join RecipeShoppingListItem rsh on rsh.ShoppingListId = sh.ShoppingListId
	inner join Recipe r on rsh.RecipeId = r.Id
	union 
	select * from RecipesInOwnedShoppingLists
go

-- Shows all accounts and recipes that they can currently cook (even if they are not in the shopping list).
create view RecipesThatCanBeCooked as
  select 
	a.Email as [Account],
	r.[Name] as [RecipeName]
  from Recipe r, ShoppingList l
  inner join Account a on a.Id = l.OwnerId
  where not exists (
	select * from Ingredient i
	left join FoodstuffShoppingListItem fi on (fi.ShoppingListId = l.Id and fi.FoodstuffId = i.FoodstuffId)
	where i.RecipeId = r.Id and fi.FoodstuffId is null
  )
go

-- Shows all accounts that have shared shopping list.
create view SharedShoppingLists as
	select 
		a.Email as [Owner],
		shwith.Email as [SharedWith]
	from Account a
	inner join ShoppingList sl on sl.OwnerId = a.Id
	inner join SharedShoppingList sh on sh.ShoppingListId = sl.Id
	inner join Account shwith on shwith.Id = sh.AccountId
go

-- Shows all recipes with additional info
create view RecipeIndex as
	select
		r.[Name] as [Name],
		case 
			when r.[Url] is null then 'Internal'
			when r.[Url] is not null then r.[Url]
		end as [Source],
		r.Difficulty as [Difficulty],
		r.PersonCount as [Serves],
		r.CookingTime as [CookingTime],
		ins.Count as [IngredientCount]
	from Recipe r
	inner join (
		select count(*) as Count, RecipeId as RecipeId from Ingredient
		group by RecipeId
	) ins
	on ins.RecipeId = r.Id
go

-- Shows foodstuff with additional info
create view FoodstuffIndex as
	select
		f.[Name] as [Name],
		ISNULL(ins.Count, 0) as [UsageInRecipes]
	from Foodstuff f
	left join (
		select count(*) as Count, FoodstuffId as FoodstuffId from Ingredient
		group by FoodstuffId
	) ins
	on ins.FoodstuffId = f.Id
go

-- Admin/Statistics views

-- Sorted list of foodstuffs based on in how many shopping lists they are.
create view MostBoughtFoodstuffs as
	select top (100)
		f.[Name] as [FoodstuffName],
		count(*) as [TimesUsed]
	from FoodstuffShoppingListItem li
	inner join Foodstuff f on f.Id = li.FoodstuffId
	group by f.[Name]
	order by count(*) desc
go

-- Sorted list of recipes based on in how many shopping lists they are.
create view MostBoughtRecipes as
	select top (100) 
		li.RecipeId as [RecipeId],
		r.[Name] as [RecipeName],
		count(*) as [TimesUsed]
	from RecipeShoppingListItem li
	join Recipe r on r.Id = li.RecipeId
	group by li.RecipeId, r.[Name]
	order by count(*) desc
go

-- Sorted list of foodstuff and their importance (usage in recipes)
create view MostUsedFoodstuffs as
	select top (100)
		f.[Name] as [FoodstuffName],
		count(*) as [TimesUsedInRecipe]
	from Ingredient i
	inner join Foodstuff f on f.Id = i.FoodstuffId
	group by f.[Name]
	order by count(*) desc
go