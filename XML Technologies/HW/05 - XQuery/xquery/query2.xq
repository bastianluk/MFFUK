(: Prepare an XHTML table for the cheapest rates for each enterprise (pair of enterprise name and rate with its name and amount(~price)) and order them in ascending order :)
declare function local:getSmallestRate($enterprise as node()*)
    as node()* 
{
    let $rates := $enterprise//rate
    let $minAmount := fn:min($rates/@amount)
    return $rates[@amount = $minAmount]
};

<table>
    <tr>
        <th>Enterprise</th>
        <th>Rate</th>
        <th>Amount</th>
    </tr>
{
    let $enterprises := fn:doc('../data.xml')//enterprise
    for $enterprise in $enterprises
    let $smallestRate := local:getSmallestRate($enterprise)
    return
    <tr>
        <td>{ $enterprise/name/text() }</td>
        <td>{ $smallestRate/@name }</td>
        <td>{ $smallestRate/@amount }</td>
    </tr>
}
</table>