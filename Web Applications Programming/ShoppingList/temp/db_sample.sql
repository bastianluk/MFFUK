-- phpMyAdmin SQL Dump
-- version 4.4.10
-- http://www.phpmyadmin.net
--
-- Host: localhost:3306
-- Generation Time: Dec 02, 2018 at 12:37 AM
-- Server version: 5.7.21-log
-- PHP Version: 7.1.7

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `shopping_list_webapp_assignment`
--

-- --------------------------------------------------------

DROP TABLE IF EXISTS `list`;
DROP TABLE IF EXISTS `items`;

--
-- Table structure for table `items`
--

CREATE TABLE IF NOT EXISTS `items` (
  `id` int(10) unsigned NOT NULL COMMENT 'ID of the item that can appear on the list.',
  `name` varchar(100) COLLATE utf8mb4_bin NOT NULL COMMENT 'Name of the item.'
) ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='A lookup table of all items that an be added to a shopping list.';

--
-- Dumping data for table `items`
--

INSERT INTO `items` (`id`, `name`) VALUES
(4, 'beer'),
(1, 'bread'),
(2, 'butter'),
(5, 'chocolate'),
(9, 'instant soup'),
(3, 'milk'),
(6, 'pork chops'),
(8, 'potatoes'),
(7, 'tomatoes');

-- --------------------------------------------------------

--
-- Table structure for table `list`
--

CREATE TABLE IF NOT EXISTS `list` (
  `id` int(10) unsigned NOT NULL COMMENT 'ID of the list item.',
  `item_id` int(10) unsigned NOT NULL COMMENT 'FK to the items table.',
  `amount` int(11) NOT NULL COMMENT 'Required amount (how much we need of this particular item).',
  `position` int(11) NOT NULL COMMENT 'Shopping list order.'
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

--
-- Dumping data for table `list`
--

INSERT INTO `list` (`id`, `item_id`, `amount`, `position`) VALUES
(1, 1, 3, 1),
(2, 2, 4, 2),
(3, 3, 1, 3),
(4, 4, 5, 4);

--
-- Indexes for dumped tables
--

--
-- Indexes for table `items`
--
ALTER TABLE `items`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `name` (`name`);

--
-- Indexes for table `list`
--
ALTER TABLE `list`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `item_id` (`item_id`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `items`
--
ALTER TABLE `items`
  MODIFY `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID of the item that can appear on the list.',AUTO_INCREMENT=10;
--
-- AUTO_INCREMENT for table `list`
--
ALTER TABLE `list`
  MODIFY `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID of the list item.',AUTO_INCREMENT=5;
--
-- Constraints for dumped tables
--

--
-- Constraints for table `list`
--
ALTER TABLE `list`
  ADD CONSTRAINT `items_fk` FOREIGN KEY (`item_id`) REFERENCES `items` (`id`);

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
