-- Register tests

-- Create new account

execute Register 'Adam', 'Novak', 'adam.novak@gmail.com'

-- See that new user is added with his shopping list
select * from Account where Email = 'adam.novak@gmail.com'
select * from ShoppingList l
inner join Account a ON a.Id = l.OwnerId
where a.Email = 'adam.novak@gmail.com'

-- Try to create duplicate account

begin try
execute Register 'Adam', 'Novak', 'adam.novak@gmail.com'
print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch
