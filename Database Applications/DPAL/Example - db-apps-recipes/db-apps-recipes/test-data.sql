-- Add data which are expected to be provided by the platform, not by the user (Recipes, Foodstuffs,...)

insert into Foodstuff ([Name]) values ('Tomato');
declare @Tomato int
set @Tomato = @@IDENTITY 
insert into Foodstuff ([Name]) values ('Onion');
declare @Onion int
set @Onion = @@IDENTITY 
insert into Foodstuff ([Name]) values ('Carrot'); 
insert into Foodstuff ([Name]) values ('Beef'); 
insert into Foodstuff ([Name]) values ('Flour');
declare @Flour int
set @Flour = @@IDENTITY 
insert into Foodstuff ([Name]) values ('Milk'); 
declare @Milk int
set @Milk = @@IDENTITY
insert into Foodstuff ([Name]) values ('Butter'); 
declare @Butter int
set @Butter = @@IDENTITY
insert into Foodstuff ([Name]) values ('Egg'); 
declare @Egg int
set @Egg = @@IDENTITY
insert into Foodstuff ([Name]) values ('Cheddar cheese'); 
declare @Cheddar int
set @Cheddar = @@IDENTITY

insert into Recipe ([Name], [Url], ImageUrl, PersonCount, [Description], CookingTime, Difficulty)
values ('Cheese omellete', 'https://omellete.com', NULL, 2, 'Step 1, step 2, ...', '00:10', 2);
declare @Omellete int
set @Omellete = @@IDENTITY
insert into Ingredient (RecipeId, FoodstuffId, Amount, Unit) values (@Omellete, @Egg, 2, 'pieces')
insert into Ingredient (RecipeId, FoodstuffId, Amount, Unit) values (@Omellete, @Cheddar, 100, 'grams')

insert into Recipe ([Name], [Url], ImageUrl, PersonCount, [Description], CookingTime, Difficulty)
values ('Cheese Bechamel sauce', 'https://bechamel.com', NULL, 2, 'Step 1, step 2, ...', '00:50', 2);
declare @Bechamel int
set @Bechamel = @@IDENTITY
insert into Ingredient (RecipeId, FoodstuffId, Amount, Unit) values (@Bechamel, @Flour, 40, 'grams')
insert into Ingredient (RecipeId, FoodstuffId, Amount, Unit) values (@Bechamel, @Butter, 40, 'grams')
insert into Ingredient (RecipeId, FoodstuffId, Amount, Unit) values (@Bechamel, @Milk, 500, 'milliliters')
insert into Ingredient (RecipeId, FoodstuffId, Amount, Unit) values (@Bechamel, @Cheddar, 100, 'grams')

insert into Recipe ([Name], [Url], ImageUrl, PersonCount, [Description], CookingTime, Difficulty)
values ('Tomato salad', 'https://salad.com', NULL, 2, 'Step 1, step 2, ...', '00:10', 2);
declare @TomatoSalad int
set @TomatoSalad = @@IDENTITY
insert into Ingredient (RecipeId, FoodstuffId, Amount, Unit) values (@TomatoSalad, @Tomato, 2, 'pieces')
insert into Ingredient (RecipeId, FoodstuffId, Amount, Unit) values (@TomatoSalad, @Onion, 1, 'pieces')
