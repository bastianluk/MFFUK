(: Return an estate element representing the relationship of owning an enterprise - it is a pair of owners name and string of the names of enterprises he/she owns:)
let $owners := fn:doc('../data.xml')//owner
for $owner in $owners
let $enterprises := fn:doc('../data.xml')//enterprise[@ownerId-ref = $owner/@oId]
where fn:count($enterprises) > 0
return element estate {
	element ownerName {
        text { $owner/name }
    },
    element enterprisesOwned {
	    fn:string-join($enterprises/name, ", ")
    }
}