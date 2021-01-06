/**
 * Data model for loading the work hour categories and fileld hours.
 * The model implements internal cache, so the data does not have to be
 * loaded every time from the REST API.
 */
class DataModel
{
    ApiUrl;
    Cache;

    /**
     * Initialize the data model with given URL pointing to REST API.
     * @param {string} apiUrl Api URL prefix (without the query part).
     */
    constructor(apiUrl)
    {
        this.ApiUrl = apiUrl;
        this.Cache = null;
    }


    /**
     * Retrieve the data and pass them to given callback function.
     * If the data are available in cache, the callback is invoked immediately (synchronously).
     * Otherwise the data are loaded from the REST API and cached internally.
     * @param {Function} callback Function which is called back once the data become available.
     *    The callback receives the data (as array of objects, where each object
     *    holds `id`, `caption`, and `hours` properties).
     *    If the fetch failed, the callback is invoked with two arguments,
     *    first one (data) is null, the second one is error message
     */
    getData(callback)
    {
        if (this.Cache !== null)
        {
            let cachedValues = Object.values(this.Cache);
            callback(cachedValues);
            return;
        }

        let emptyParameters = {};
        let actionResult = this.callAction('GET', emptyParameters);
        let test = actionResult.then(categories =>
        {
            let count = categories.length;
            let completeCategories = []
            for (let category of categories)
            {
                handleCategory(callback, completeCategories, category, count)
            }
        });

        test.catch(reason => callback(null, reason));
    }

    handleCategory(callback, completeCategories, category, categoryCount)
    {
        let parameters = {
            action: 'hours',
            id: category.id
        };
        let actionResult = this.callAction('GET', parameters);
        let completeCategoryActionResult = actionResult.then(completeCategory => this.completeCategoryAction(callback, completeCategories, completeCategory, categoryCount));

        completeCategoryActionResult.catch(reason => callback(null, reason));
    }

    completeCategoryAction(callback, completeCategories, completeCategory, categoryCount)
    {
        completeCategories.push(completeCategory)
        let allCategoriesDone = completeCategories.length === categoryCount;
        if (allCategoriesDone)
        {
            this.addCategoriesToCache(completeCategories);
            callback(completeCategories);
        }
    }

    addCategoriesToCache(categories)
    {
        this.Cache = {};
        for (const category of categories)
        {
          this.Cache[category.id] = category;
        }
    }


    /**
     * Invalidate internal cache. Next invocation of getData() will be forced to load data from the server.
     */
    invalidate()
    {
        this.Cache = null;
    }


    /**
     * Modify hours for one record.
     * @param {number} id ID of the record in question.
     * @param {number} hours New value of the hours (m)
     * @param {Function} callback Invoked when the operation is completed.
     *    On failutre, one argument with error message is passed to the callback.
     */
    setHours(id, hours, callback = null)
    {
        let parameters = {
            action: 'hours',
            id,
            hours
        };
        let actionResult = this.callAction('POST', parameters);
        let cachedResult = actionResult.then(() =>
        {
            if (this.cache !== null) {
              this.cache[id].hours = hours
            }
            callback()
        });
        cachedResult.catch(callback);
    }

    callAction(method, parameters)
    {
        let url = this.createUrl(parameters);

        let promise = new Promise((resolve, reject) =>
        {
            let fetchResult = fetch(url, { method });
            let reseolvedResult = fetchResult.then(response =>
            {
                if (!response.ok)
                {
                    this.handleFetchErrorResult(response);
                    return;
                }

                let jsonResponse = response.json().then(json => this.jsonResultHandler(json, resolve, reject));
                jsonResponse.catch(reject);
          })

          reseolvedResult.catch(reject);
        });

        return promise;
    }

    createUrl(parameters)
    {
        let url = this.ApiUrl + '?';
        let parameterKVPs = Object.entries(parameters);
        for (let [key, value] of parameterKVPs)
        {
          url += key + '=' + value + '&';
        }
        // Remove the trailing '&'
        return url.slice(0, -1);
    }

    handleFetchErrorResult(response)
    {
        let errorResponse = response.text().then(text =>
        {
            throw new Error(text);
        });
        errorResponse.catch(reject);
    }

    jsonResultHandler(json, resolve, reject)
    {
        if (!json.ok)
        {
            reject(json.error);
            return;
        }

        let hasOwnPayload = json.hasOwnProperty('payload');
        hasOwnPayload ? resolve(json.payload) : resolve();
    }
}


// In nodejs, this is the way how export is performed.
// In browser, module has to be a global varibale object.
module.exports = { DataModel };
