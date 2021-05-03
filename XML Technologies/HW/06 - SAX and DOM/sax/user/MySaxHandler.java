package user;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;
/**
 * Main class for SAX parsing
 * @author XML Technologies
 */
/**
 * Out custom content handler for managing SAX events
 * @see http://www.saxproject.org/apidoc/org/xml/sax/ContentHandler.html
 */
public class MySaxHandler extends DefaultHandler {
    
    // Reference to the locator
    Locator locator;

    OutputStreamWriter outputStreamWriter;
    
    List<Integer> prices;
    
    String currentEnterprise;
    
    Integer roomsWithDesiredCapacityCount;
    
    List<String> roomCategoryIds;
    
    Integer roomTypesWithMinibarCount;
    
    Boolean inFeature;

    public StringBuffer buffer;


    public MySaxHandler () {
        this.outputStreamWriter = new OutputStreamWriter(System.out);
        prices = new ArrayList<>();  
        currentEnterprise = "";
        roomsWithDesiredCapacityCount = 0;
        roomCategoryIds = new ArrayList<>();
        roomTypesWithMinibarCount = 0;
        inFeature = false;
        buffer = new StringBuffer();
    }
    
    /**
     * Stores reference to the locator
     * @param Locator locator Location in the source file
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        this.locator = locator;
    }
    
    /**
     * Document beginning handler
     * @throws SAXException
     */
    @Override
    public void startDocument() throws SAXException {
        // ...
    }
    
    /**
     * Document end handler
     * @throws SAXException
     */
    @Override
    public void endDocument() throws SAXException {
        // add the print characteristics:
        Double averagePrice = calculateAveragePrice(prices);
        printLine("Average rate price accross hotels: " + averagePrice.toString());
        
        printLine("Number of times a sauna is defined as a feature: " + roomTypesWithMinibarCount.toString());
        
        printLine("Number of rooms with capacity at least 3 based on the room type accross the chain: " + roomsWithDesiredCapacityCount.toString());
    }
    
    /**
     * Element beginning handler
     * @param uri URI of the element namespace (empty when no namespace)
     * @param localName Local name of the element (never empty)
     * @param qName Qualified name
     * @param atts Attributes
     * @throws SAXException
     */
    @Override
    public void startElement(
        String uri, String localName, String qName, Attributes atts
    ) throws SAXException {
        
        if ("rate".equals(qName)) {
            evalChar1(atts);
        }
        
        if ("enterprise".equals(qName)) {
            currentEnterprise = atts.getValue("name");
        }
        
        if ("roomType".equals(qName)) {
            Integer capacity = Integer.parseInt(atts.getValue("capacity"));
            if (capacity >= 3) {
                String id = atts.getValue("rtId");
                roomCategoryIds.add(id);
            }
        }
        
        if ("room".equals(qName)) {
            evalChar3(atts);
        }
        
        if ("feature".equals(qName)) {
            inFeature = true;
        }
        
        buffer = new StringBuffer();
       
    }
    
    public void evalChar1(Attributes atts)
    {
        Integer amount = Integer.parseInt(atts.getValue("amount"));
        prices.add(amount);
    }
    
    public void evalChar3(Attributes atts) {
        String rtIdref = atts.getValue("rtId-reference");
        if (roomCategoryIds.contains(rtIdref)) {
            roomsWithDesiredCapacityCount += 1;
        }        
    }
    
    /**
     * Element end handler
     * @param uri URI of the element namespace (empty when no namespace)
     * @param localName Local name of the element (never empty)
     * @param qName Qualified name
     * @throws SAXException
     */
    @Override
    public void endElement(
        String uri, String localName, String qName
    ) throws SAXException {
        if ("feature".equals(qName)) {
            inFeature = false;
        }
        
        if (inFeature && "name".equals(qName)) {
            if ("Minibar".equals(buffer.toString())) {
                //evalChar2
                roomTypesWithMinibarCount += 1;
            }
        }
    }
    
    /**
     * Character data handler
     *  - Text content may be delivered within multiple events, not just one
     * @param chars Array with character data
     * @param start Index of the start position in the array
     * @param length Number of characters to read from the array
     * @throws SAXException
     */
    @Override
    public void characters(
        char[] chars, int start, int length
    ) throws SAXException {
        
        String s = new String(chars, start, length).trim();
        
        if (s.length() > 0) {            
            buffer.append(s);            
        }
    }
    
    /**
     * Namespace declaration beginning handler
     * @param prefix Prefix of the namespace
     * @param uri URI of the namespace
     * @throws SAXException
     */
    @Override
    public void startPrefixMapping(
        String prefix, String uri
    ) throws SAXException {
        // ...
    }
    
    /**
     * Namespace declaration end handler
     * @param prefix Prefix of the namespace
     * @throws SAXException
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        // ...
    }

    /**
     * Ignorable whitespace characters handler
     * @param chars Array with character data
     * @param start Index of the start position in the array
     * @param length Number of characters to read from the array
     * @throws SAXException
     */
    @Override
    public void ignorableWhitespace(
        char[] chars, int start, int length
    ) throws SAXException {
        // ...
    }
    
    /**
     * Processing instruction handler
     * @param target Processing instruction target
     * @param data Processing instruction data
     * @throws SAXException
     */
    @Override
    public void processingInstruction(
        String target, String data
    ) throws SAXException {
        // ...
    }
    
    /**
     * Unprocessed entity handler
     * @param name Name of the skipped entity
     * @throws SAXException
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        // ...
    }
    
    public void printLine(String what) {
        // https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html
        try {
            outputStreamWriter.write(what + "\r\n");
            outputStreamWriter.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    public Double calculateAveragePrice(List<Integer> listOfPrices) {
        Double sum = 0.0;
        for (int i: listOfPrices) {
            sum += i;
        }
        Double average = sum / listOfPrices.size();
        return average;
    }
}