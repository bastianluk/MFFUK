<h2>Shopping list</h2>

<datalist id="knowItems">
  <option value="Edge">
  <option value="Firefox">
  <option value="Chrome">
  <option value="Opera">
  <option value="Safari">
</datalist>


<h2>Add new item</h2>
<form action="?page=add/index" method="post" id="newItem">
    <div class="input">
        <label for="name">Name:</label>
        <input list="knowItems" name="name" id="name">
    </div>
    <div class="input">
        <label for="amount">Amount:</label>
        <input type="number" name="amount" id="amount" min="0">
    </div>

    <div class="input">
        <input type="submit" value="Add"/>
    </div>
</form>


<p>
	The assignment is designed to introduce Front Controller design pattern to you.
	Your objective is to design a simple page that utilizes this particular pattern
	to desing a simple web page.
</p>

<p>
	Please, be advised that your solution will be tested in ReCodEx. Try not to be
	excessively creative in your implementation. For instance, there is no need for
	using cookies or manually set headers in any way. On the other hand, if you need
	to set HTTP response code, use standard PHP builtin
	<a href="http://php.net/manual/en/function.http-response-code.php"><code>http_response_code()</code></a>
</p>

<p>
	The student results page is here merely to test whether your front controler sanitizes query parameters correctly.
</p>