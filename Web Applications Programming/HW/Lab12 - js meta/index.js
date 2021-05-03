const fs = require('fs');
var solution = require('./solution.js');

try {
	const fileName = process.argv.pop();
	const objs = JSON.parse(fs.readFileSync(fileName));
	const debug = process.argv.pop() === '--debug';

	if (!Array.isArray(objs) || objs.length == 0) {
		throw new Error(`File ${fileName} does not contain expected data.`);
	}

	// Reconstruct nested object references...
	objs.forEach(obj => {
		obj.similar = obj.similar.map(i => objs[i]);
		obj.created = new Date(obj.created*1000);
	});

	const res = solution.preprocessGalleryData(objs);
	console.log(JSON.stringify(
		res.map(
			block => block.map(({ url, title, created, similar }) => ({
				url,
				title,
				created: debug ? created.toISOString() : Math.floor(created.getTime() / 1000),
				similar: similar.map(({ url }) => url).sort()	// save only url (and sort them to make it deterministic)
			}))
		),
	undefined, debug ? 2 : undefined));
}
catch (e) {
	console.log("Error: " + e.message);
	process.exit(1);
}
