-- TODO

-- Testy integritn�ch omezen�
-----------------------------


-- Testy trigger�
-----------------


-- Testy procedur a funkc�
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





-- Testy pohled�
----------------