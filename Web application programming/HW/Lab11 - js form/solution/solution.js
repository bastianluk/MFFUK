/**
 * Example of a local function which is not exported. You may use it internally in processFormData().
 * This function verifies the base URL (i.e., the URL prefix) and returns true if it is valid.
 * @param {*} url
 */
function verifyBaseUrl(url)
{
    return Boolean(url.match(/^https:\/\/[-a-z0-9._]+([:][0-9]+)?(\/[-a-z0-9._/]*)?$/i));
}

/**
 * Example of a local function which is not exported. You may use it internally in processFormData().
 * This function verifies the relative URL (i.e., the URL suffix) and returns true if it is valid.
 * @param {*} url
 */
function verifyRelativeUrl(url)
{
    return Boolean(url.match(/^[-a-z0-9_/]*([?]([-a-z0-9_\]\[]+=[^&=]*&)*([-a-z0-9_\]\[]+=[^&=?#]*)?)?$/i));
}


/**
 * Main exported function that process the form and yields the sanitized data (or errors).
 * @param {*} formData Input data as FormData instance.
 * @param {*} errors Object which collects errors (if any).
 * @return Serialized JSON containing sanitized form data.
 */
function processFormData(formData, errors)
{
    /*
     * Your code goes here ...
     */
}


// In nodejs, this is the way how export is performed.
// In browser, module has to be a global varibale object.
module.exports = { processFormData };
