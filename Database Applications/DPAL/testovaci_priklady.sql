-- Integrity constraints tests
------------------------------

-- Procedur and function tests
------------------------------

-- Cannot Register invalid format game

begin try
  execute RegisterGame 'Dead by Daylight', '1v4'
  print 'Test failed'
end try
begin catch
	print 'Test succeeded'
end catch

-- Can register a valid format game

begin try
  execute RegisterGame 'Quake Tournament', '1v1'
  print 'Test succeeded'
end try
begin catch
	print 'Test failed'
end catch


-- View tests
-------------

