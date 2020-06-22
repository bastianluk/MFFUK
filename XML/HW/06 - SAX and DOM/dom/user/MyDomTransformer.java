package user;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class MyDomTransformer {
    
    public void transform(Document doc) {
        
        // Increase all the prices by 50% (as a result of a crisis).
        NodeList rateList = doc.getElementsByTagName("rate");
        for (int i = rateList.getLength() - 1; i >= 0; --i) {
            
            Element rate = (Element)rateList.item(i);
            int amount = Integer.parseInt(rate.getAttribute("amount"));
            amount = (int)(amount * 1.5);
            
            rate.setAttribute("amount", Integer.toString(amount));            
        }
        
        // Add parking as a new product in a separate productCategory for each enterprise's visit.
        NodeList visits = doc.getElementsByTagName("visit");
        for (int i = visits.getLength() - 1; i >= 0; --i) {
            
            Element visit = (Element)visits.item(i);
            
            Element newProductCategory = doc.createElement("productCategory");
            newProductCategory.setAttribute("name", "Services");
            
            Element parking = doc.createElement("product");
            parking.setAttribute("name", "Parking");
            parking.setAttribute("amount", "10");
            
            newProductCategory.appendChild(parking);
            
            visit.appendChild(newProductCategory);
        }
    }
}
