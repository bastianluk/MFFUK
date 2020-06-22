<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:fn="http://www.w3.org/2005/xpath-functions"
>

    <xsl:output
        method="html"
        version="4.0"
        encoding="UTF-8"
        indent="yes"
        doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
        doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
    />

    <!-- Base for the "website" made by this XSLT. -->
    <xsl:template match="/">
        <html>
            <head>
                <title>Hotel Chain Reporter</title>
                <style>
                    th { background-color: #AAAAAA; }
                </style>
            </head>
            <body>
                <xsl:apply-templates select="//enterprises"/>                
            </body>
        </html>
    </xsl:template>

    <!-- The body of the website when it comes to enterprises - "hub" that makes sure all the tables for enterprises are ready. -->
    <xsl:template match="enterprises">
        <xsl:copy>
            <xsl:apply-templates select="//enterprises" mode="Main"/>
        </xsl:copy>
        <xsl:copy>
            <xsl:apply-templates select="//enterprises" mode="RoomTypes"/>
        </xsl:copy>
    </xsl:template>


    <!-- Main table reporting the enterprises in a chain - their name, owners name, number of rooms and the lowest rate and its amount. -->
    <xsl:template match="enterprises" mode="Main">
        <h2>Hotels</h2>
        <table>
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Owner Name</th>
                    <th>Number of Rooms</th>
                    <th>Lowest Rate (Amount)</th>
                </tr>
            </thead>
            <tbody>
                <xsl:apply-templates select="enterprise" mode="MainInternal"/>
            </tbody>
        </table>
    </xsl:template>
    <!-- Internals for the main table - gets the name, calls a template to get owner name, counts the rooms in the enterprise and calls a teplate to get the lowest rates name and mount. -->
    <xsl:template match="enterprises/enterprise" mode="MainInternal">
        <tr>
            <td>
                <xsl:value-of select="name"/>
            </td>
            <td>
                <xsl:call-template name="enterpriseOwnerName">
                    <xsl:with-param name="ownerId" select="@ownerId-ref"/>
                </xsl:call-template>
            </td>
            <td align="center">
                <xsl:apply-templates select="rooms"/>
            </td>
            <td>
                <xsl:call-template name="lowestRate">
                    <xsl:with-param name="visit" select="visit"/>
                </xsl:call-template>
            </td>
        </tr>
    </xsl:template>

    <!-- Get the name of the owner of an enterprise (if it exists). -->
    <xsl:template name="enterpriseOwnerName">
        <xsl:param name="ownerId" select="@ownerId-ref"/>
        <xsl:variable name="ownerName" select="//owner[@oId = $ownerId]/name"/>
        <xsl:choose>
            <xsl:when test="$ownerName">
                <xsl:value-of select="$ownerName"/>
            </xsl:when>
            <xsl:otherwise>
                <i>Unknown</i>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    <!-- Count all the rooms -->
    <xsl:template match="//rooms">
        <xsl:value-of select="count(room)" />
    </xsl:template>
    <!-- Get the lowest rate's name and price (amount) of a visit (from an enteprise). -->
    <xsl:template name="lowestRate">
        <xsl:param name="visit" select="visit"/>
        <xsl:variable name="rates" select="visit/rate"/>
        <xsl:for-each select="$rates">
            <xsl:sort select="@amount" data-type="number" order="ascending"/>
            <xsl:if test="position()=1">
                <xsl:value-of select="concat(@name, ' (',@amount, ')')"/>
            </xsl:if>
        </xsl:for-each>
    </xsl:template>



    <!-- Per enterprise table of room types with its name, capacity, total rooms available and list of features. -->
    <xsl:template match="enterprises" mode="RoomTypes">
        <xsl:apply-templates select="enterprise" mode="RoomTypesInternal"/>
    </xsl:template>
    <!-- Per enterprise table of room types with its name, capacity, total rooms available and list of features. -->
    <xsl:template match="enterprise" mode="RoomTypesInternal">
        <br/>
        <h3>
            <xsl:value-of select="name"/>
        </h3>
        <table>
            <thead>
                <tr>
                    <th>Room type</th>
                    <th>Capacity</th>
                    <th>Total Rooms Available</th>
                    <th>Features</th>
                </tr>
            </thead>
            <tbody>
                <xsl:apply-templates select="roomTypes/roomType"/>
            </tbody>
        </table>
    </xsl:template>

    <!--Internals for the per enterprise table of room types - gets the name, capacity, calls for the total number of rooms of each room type, and calls for the list of features. -->
    <xsl:template match="enterprise/roomTypes/roomType">
        <tr>
            <td>
                <xsl:value-of select="name"/>
            </td>
            <td align="center">
                <xsl:value-of select="@capacity"/>
            </td>
            <td align="center">
                <xsl:call-template name="roomCountOfType">
                    <xsl:with-param name="roomTypeId" select="@rtId"/>
                </xsl:call-template>
            </td>
            <td>
                <ul>
                    <xsl:apply-templates select="features/feature" mode="listBullet"/>
                </ul>
            </td>
        </tr>
    </xsl:template>

    <!-- Gets the number of rooms (if there are any) of a certain room type based on its Id. -->
    <xsl:template name="roomCountOfType">
        <xsl:param name="roomTypeId" select="@rtId"/>
        <xsl:variable name="number" select="count(//room[@rtId-reference = $roomTypeId])"/>
        <xsl:choose>
            <xsl:when test="$number">
                <xsl:value-of select="$number"/>
            </xsl:when>
            <xsl:otherwise>
                <i>-</i>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    <!-- Gets one bullet in a litst for a feature. -->
    <xsl:template match="features/feature" mode="listBullet">
        <li>
            <xsl:value-of select="name"/>
        </li>
    </xsl:template>

</xsl:stylesheet>