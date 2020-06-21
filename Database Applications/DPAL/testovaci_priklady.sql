-- TODO

-- Testy integritních omezení
-----------------------------


-- Testy triggerù
-----------------


-- Testy procedur a funkcí
--------------------------

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





-- Testy pohledù
----------------