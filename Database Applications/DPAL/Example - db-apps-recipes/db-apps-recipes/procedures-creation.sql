-- Registration of new Account.
create procedure Register
  @FirstName nvarchar(32),
  @Lastname nvarchar(128),
  @Email nvarchar(128)
as
	begin transaction;
		begin try
			insert into Account (FirstName, LastName, Email) values (@FirstName, @Lastname, @Email);

			declare @AccountId int;
			set @AccountId = @@IDENTITY;

			-- Insert default shopping list.
			insert into ShoppingList (OwnerId) values (@AccountId)
		end try
		begin catch
			rollback transaction;
			throw;
		end catch
  commit;
go

-- Add foodstfuff to shopping list by name, will create the foodstuff if does not exists.
create procedure AddFoodstuffToShoppingList
	@ShoppingListId int,
	@FoodstuffName nvarchar(128)
as
	begin transaction;
		begin try
			declare @FoodstuffId int;

			if (exists (select Id from Foodstuff where [Name] = @FoodstuffName))
			begin
				select @FoodstuffId = Id from Foodstuff where [Name] = @FoodstuffName;
			end
			else
			begin 
				insert into Foodstuff ([Name]) values (@FoodstuffName);
				set @FoodstuffId = @@IDENTITY;
			end

			insert into FoodstuffShoppingListItem (ShoppingListId, FoodstuffId) values (@ShoppingListId, @FoodstuffId);
		end try
		begin catch
			rollback transaction;
			throw;
		end catch
	commit;
go

-- Remove foodstuff from shopping list, will do nothing if foodstuff is not there.
create procedure RemoveFoodstuffToShoppingList
	@ShoppingListId int,
	@FoodstuffName nvarchar(128)
as
	delete i
	from FoodstuffShoppingListItem i
	inner join Foodstuff f on f.Id = i.FoodstuffId
	where i.ShoppingListId = @ShoppingListId and f.[Name] = @FoodstuffName
go

-- Share the shopping list with another account.
create procedure Share
	@ShoppingListId int,
	@AccountId int
as
	if (exists (
		select * from ShoppingList 
		where Id = @ShoppingListId and OwnerId = @AccountId))
	begin
		throw 60003, 'Cannot share shopping list with the owner.', 0;
	end
		
	insert into SharedShoppingList (ShoppingListId, AccountId) values (@ShoppingListId, @AccountId)
go

-- Add recipe to shopping list.
create procedure AddRecipeToShoppingList
	@ShoppingListId int,
	@RecipeId int
as
	insert into RecipeShoppingListItem (ShoppingListId, RecipeId) values (@ShoppingListId, @RecipeId)
go

-- Removes recipe from shopping list.
create procedure RemoveRecipeFromShoppingList
	@ShoppingListId int,
	@RecipeId int
as
	delete RecipeShoppingListItem 
	where ShoppingListId = @ShoppingListId and RecipeId = @RecipeId
go

-- Clears shopping list from both recipes and foodstuffs.
create procedure ClearShoppingList
	@ShoppingListId int
as
	begin transaction;
		begin try
			delete from RecipeShoppingListItem
			where ShoppingListId = @ShoppingListId;

			delete from FoodstuffShoppingListItem
			where ShoppingListId = @ShoppingListId;
		end try
		begin catch
			rollback transaction;
			throw;
		end catch
	commit;
go

-- Search recipes by prefix.
create function SearchRecipes(
	@Term nvarchar(128)
) returns table
as
	return (
		select *
		from Recipe
		where [Name] like (@Term + '%')
	)
go

-- Search foodstuffs by prefix.
create function SearchFoodstuffs(
	@Term nvarchar(128)
) returns table
as
	return (
		select *
		from Foodstuff
		where [Name] like (@Term + '%')
	)
go

-- Cook a recipe will remove both recipe and all ingredients required for it from the shopping list.
-- Fails if there is not enough resources to cook the recipe.
create procedure Cook
	@ShoppingListId int,
	@RecipeId int
as
	set transaction isolation level serializable -- Foodstuff can be removed from shopping list after the check.
	begin transaction;
		begin try
			-- Check that the recipe is in shopping list.
			if (not exists (select * from RecipeShoppingListItem where ShoppingListId = @ShoppingListId and RecipeId = @RecipeId))
			begin
				throw 60004, 'There is no such recipe in given shopping list.', 0;
			end

			-- Check if there is enough resources to cook the recipe in given shopping list.
			if (exists (
				select i.FoodstuffId from Ingredient i
				left join FoodstuffShoppingListItem li 
				on li.ShoppingListId = @ShoppingListId and li.FoodstuffId = i.FoodstuffId
				where 
					i.RecipeId = @RecipeId and
					li.FoodstuffId is null
			))
			begin
				throw 60005, 'There is not enough resources in order to make the ingredient.', 0;
			end

			delete li from FoodstuffShoppingListItem li
			where exists(
				select * from Ingredient i 
				where 
					i.RecipeId = @RecipeId and
					i.FoodstuffId = li.FoodstuffId
			);

			delete from RecipeShoppingListItem where RecipeId = @RecipeId;
		end try
		begin catch
			rollback transaction;
			throw;
		end catch
	commit;
