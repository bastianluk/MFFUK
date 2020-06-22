(: Finds an enterprise (its name and basic contact info) which has all rates below the average of the chain. :)
let $enterprises := fn:doc('../data.xml')//enterprise
for $enterprise in $enterprises
where every $rateAmount in $enterprise//visit/rate/@amount satisfies $rateAmount < fn:avg($enterprises//visit/rate/@amount)
return element enterprise {
    element name {
        text { $enterprise/name }
    },
    element city { 
        text { $enterprise/contact/address/city }
    },
    element website { 
        text { $enterprise/contact/website }
    }
}