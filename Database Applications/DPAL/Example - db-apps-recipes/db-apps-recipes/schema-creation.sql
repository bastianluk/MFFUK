/*
* Application is build around domain of recipes, ingredients, shopping lists and cooking.
* Primary goal for the user is to collect ingredients, choose recipes, be able to cook them and share it with other users.
* All use-cases are in files prefixed with test-*
* Flow of execution is :
* - schema-creation
* - view-creation
* - procedure-creation
* - index-creation
* - statistics-creations
* - test-data
*
* Now we can see different test-* files, which are independent, so can be executed in any order.
*/

/*
* Represents the user which can have shopping lists.
*/
CREATE TABLE Account (
	Id int IDENTITY(1,1)
		constraint Account_PK PRIMARY KEY,
	FirstName nvarchar(32) NOT NULL,
	LastName nvarchar(128) NOT NULL,
	Email nvarchar(128) NOT NULL
		constraint Account_Email UNIQUE (Email)
);

/*
* Simple foodstuff, which can be bought in market like Tomato, Onion etc.
*/
CREATE TABLE Foodstuff (
	Id int IDENTITY(1,1)
		constraint Foodstuff_PK PRIMARY KEY,
	[Name] nvarchar(128) NOT NULL,
		constraint Foodstuff_Name UNIQUE ([Name])
);

/*
* Aggregate of foodstuffs and recipes, which can be shared among users. By default a user has 1 shopping list.
*/
CREATE TABLE ShoppingList (
	Id int IDENTITY(1,1)
		constraint ShoppingList_PK PRIMARY KEY,
	OwnerId int
		constraint ShoppingList_FK_Owner REFERENCES Account(Id)
);

/*
* Represents shared shopping list with different user.
*/
CREATE TABLE SharedShoppingList (
	ShoppingListId int
		constraint SharedShoppingList_FK_ShoppingList REFERENCES ShoppingList(Id),
	AccountId int
		constraint SharedShoppingList_FK_Account REFERENCES Account(Id),
	constraint SharedShoppingList_PK PRIMARY KEY (ShoppingListId, AccountId)
);


/*
* Represents foodstuff in shopping list.
*/
CREATE TABLE FoodstuffShoppingListItem (
	ShoppingListId int
		constraint FoodstuffShoppingListItem_FK_ShoppingList REFERENCES ShoppingList(Id),
	FoodstuffId int
		constraint FoodstuffShoppingListItem_FK_Foodstuff REFERENCES Foodstuff(Id),
	constraint FoodstuffShoppingListItem_PK PRIMARY KEY (ShoppingListId, FoodstuffId)
);


/*
* Represents a recipe, which has multiple ingredients and other information.
*/
CREATE TABLE Recipe (
	Id int IDENTITY(1,1)
		constraint Recipe_PK PRIMARY KEY,
	[Name] nvarchar(128) NOT NULL,
	[Url] nvarchar(256)
		constraint Recipe_Url UNIQUE ([Url]),
	ImageUrl nvarchar(max),
	PersonCount int NOT NULL,
	[Description] nvarchar(max),
	CookingTime nchar(5)
		constraint Recipe_CHK_CookingTime CHECK (CookingTime like '[0-9][0-9]:[0-9][0-9]'),
	Difficulty int NULL
		constraint Recipe_CHK_Difficulty CHECK (Difficulty >= 0 AND Difficulty <= 10)
);

/*
* Represents foodstuff bound to a specific recipe with some amount information.
*/
CREATE TABLE Ingredient (
	RecipeId int
		constraint Ingredient_FK_Recipe REFERENCES Recipe(Id),
	FoodstuffId int
		constraint Ingredient_FK_Foodstuff REFERENCES Foodstuff(Id),
	Unit nvarchar(32),
	Amount int,
	constraint Ingredient_PK PRIMARY KEY (RecipeId, FoodstuffId, Unit)
)


/*
* Represents a recipe in some shopping list, which can be p[ossibly cooked.
*/
CREATE TABLE RecipeShoppingListItem (
	ShoppingListId int
		constraint RecipeShoppingListItem_FK_ShoppingList REFERENCES ShoppingList(Id),
	RecipeId int
		constraint RecipeShoppingListItem_FK_Recipe REFERENCES Recipe(Id),
	constraint RecipeShoppingListItem_PK PRIMARY KEY (ShoppingListId, RecipeId)
);