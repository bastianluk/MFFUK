//https://webik.ms.mff.cuni.cz/examples/js/jsfiddle.php?script=09-01-dom

var myList = document.getElementById("myList");
log("The list has "
    + myList.childNodes.length + " items.");

var newItem = document.createElement("li");
newItem.textContent = "Item #"
    + (myList.getElementsByTagName("li").length + 1);
myList.appendChild(newItem);

//var lis = myList.getElementsByTagName("li");
//myList.removeChild(lis[0]);



//https://webik.ms.mff.cuni.cz/examples/js/jsfiddle.php?script=09-02-css
var myList = document.getElementById("myList");
var li = myList.getElementsByTagName('li').item(1);
li.style.fontWeight = 'bold';
li.style.color = '#ff0000';

var p = document.getElementsByTagName("p")[0];
if (p.className == 'niceText')
    p.className = '';
else
    p.className = 'niceText';



//https://webik.ms.mff.cuni.cz/examples/js/jsfiddle.php?script=09-03-events
var box = document.getElementById('box');

box.addEventListener('mousemove', function(ev){
    box.textContent = ev.clientX + ", " + ev.clientY;
});

box.addEventListener('mouseleave', function(ev){
    box.textContent = 'no mouse in the box ...';
});

box.addEventListener('mouseenter', function(ev){
    log("Mouse entered the box ...");
});

box.addEventListener('mouseleave', function(ev){
    log("Mouse left the box ...");
});



//https://webik.ms.mff.cuni.cz/examples/js/jsfiddle.php?script=09-04-bubbling
var div1 = document.getElementById('div1');
var div2 = document.getElementById('div2');
var div3 = document.getElementById('div3');

div1.onclick = function(ev) {
    log("div 1 was clicked");
}

div2.onclick = function(ev) {
    log("div 2 was clicked");
    //ev.stopPropagation();
}

div3.onclick = function(ev) {
    log("div 3 was clicked");
}



//https://webik.ms.mff.cuni.cz/examples/js/jsfiddle.php?script=10-01-closure
function SecretRumours()
{
    var rumours = {        // local variable in the SecretRumours scope
        bednarek: 'Drove four people mad when supervising a software project.',
        yaghob: 'Is called Dr. Evil by some faculty staff.',
        zavoral: 'Thinks that web applications are made of <h1> and <h2> elements mostly.',
    };

    return {    // return an object containing two functions
        get: function(person) {
            return rumours[person] ? rumours[person]
                : "No rumours about '" + person + "'.";
        },
        add: function(person, rumour) {
            rumours[person] = rumour;
        }
    };
}

var rumours = SecretRumours();

log(rumours.get('bednarek'));
log(rumours.get('krulis'));
rumours.add('krulis', 'Still sleeps with his teddy bear.');
log(rumours.get('krulis'));

log(Object.keys(rumours));



//https://webik.ms.mff.cuni.cz/examples/js/jsfiddle.php?script=10-02a-prototypes
var Person = {
    name: "",
    getName: function() { return this.name; },
}

var Teacher = Object.create(Person);
Teacher.courses = [];
Teacher.printCourses = function() { log(this.courses.join(", ")); };
Teacher.teach = function() {};

var Bednarek = Object.create(Teacher);
Bednarek.name = "Bednarek";
Bednarek.courses = [ "C++", "Compilers" ];
Bednarek.teach = function() { log("Let me tell you about C++ tuples..."); };

var Yaghob = Object.create(Teacher);
Yaghob.name = "Yaghob";
Yaghob.teach = function() { log("If you make a mistake like this one, you'll end up having an ingot in caffeteria..."); };
Yaghob.courses.push("Parallel Programming");
Yaghob.courses.push("Virtualization");

var Zavoral = Object.create(Teacher);
Zavoral.name = "Zavoral";

log(Bednarek.getName());
Bednarek.teach();
Bednarek.printCourses();

log(Yaghob.getName());
Yaghob.teach();
Yaghob.printCourses();

log(Zavoral.getName());
Zavoral.teach();
Zavoral.printCourses();



//https://webik.ms.mff.cuni.cz/examples/js/jsfiddle.php?script=10-02b-prototypes-fixed
var Person = {
    getName: function() { return this.name ? this.name : ""; },
}

var Teacher = Object.create(Person);
Teacher.printCourses = function() {
    if (this.courses) log(this.courses.join(", "));
};
Teacher.teach = function() {};

var Bednarek = Object.create(Teacher);
Bednarek.name = "Bednarek";
Bednarek.courses = [ "C++", "Compilers" ];
Bednarek.teach = function() { log("Let me tell you about C++ tuples..."); };

var Yaghob = Object.create(Teacher);
Yaghob.name = "Yaghob";
Yaghob.teach = function() { log("If you make a mistake like this one, you'll end up having an ingot in caffeteria..."); };
Yaghob.courses = [];
Yaghob.courses.push("Parallel Programming");
Yaghob.courses.push("Virtualization");

var Zavoral = Object.create(Teacher);
Zavoral.name = "Zavoral";

log(Bednarek.getName());
Bednarek.teach();
Bednarek.printCourses();

log(Yaghob.getName());
Yaghob.teach();
Yaghob.printCourses();

log(Zavoral.getName());
Zavoral.teach();
Zavoral.printCourses();



//https://webik.ms.mff.cuni.cz/examples/js/jsfiddle.php?script=11-01-ajax
var httpReq = new XMLHttpRequest();
httpReq.open("GET", "/examples/js/freedive.json");
httpReq.onreadystatechange = function() {
    log("Ready state change to " + httpReq.readyState);

    // Only interested in completed requests.
    if (httpReq.readyState != 4)
        return;

    log("AJAX response status " + httpReq.status);

    // Check the response code.
    if (httpReq.status == 200) {
        log("Here goes the results of free-diving contest:");
        var results = JSON.parse(httpReq.responseText);
        results.sort((a,b) => b.time - a.time)
            .forEach((res, position) => {
                var str = (position+1) + ". " + res.name + " " + res.time + "s";
                if (res.note) str = str + " (" + res.note + ")";
                log(str);
            });
    }
    else
        alert("AJAX Error: " + httpReq.status);
}
httpReq.send();



//https://webik.ms.mff.cuni.cz/examples/js/jsfiddle.php?script=11-02-fetch
fetch("/examples/js/freedive.json")
.then(response => response.json())
.then(results =>
    results.sort((a,b) => b.time - a.time)
        .forEach((res, position) => {
            var str = (position+1) + ". " + res.name + " " + res.time + "s";
            if (res.note) str = str + " (" + res.note + ")";
            log(str);
        })
);




//https://webik.ms.mff.cuni.cz/examples/js/jsfiddle.php?script=adv-08-01-bubbling
var div0 = document.getElementById('div0');
var div1 = document.getElementById('div1');
var div2 = document.getElementById('div2');
var div3 = document.getElementById('div3');

div0.addEventListener("click", function(ev) {
    log("click bubbling down through div 0");
}, true);

div0.addEventListener("click", function(ev) {
    log("div 0 was clicked");
}, false);


div1.addEventListener("click", function(ev) {
    log("click bubbling down through div 1");
}, true);

div1.addEventListener("click", function(ev) {
    log("div 1 was clicked");
}, false);


div2.addEventListener("click", function(ev) {
    log("click bubbling down through div 2");
}, true);

div2.addEventListener("click", function(ev) {
    log("div 2 was clicked");
    //ev.stopPropagation();
}, false);


div3.addEventListener("click", function(ev) {
    log("click bubbling down through div 3");
}, true);

div3.addEventListener("click", function(ev) {
    log("div 3 was clicked");
}, false);


