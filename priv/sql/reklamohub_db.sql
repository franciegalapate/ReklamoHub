-- MySQL dump 10.13  Distrib 8.0.41, for Win64 (x86_64)
--
-- Host: localhost    Database: reklamohub_db
-- ------------------------------------------------------
-- Server version	9.1.0

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `admin`
--

DROP TABLE IF EXISTS `admin`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `admin` (
  `admin_username` varchar(50) NOT NULL,
  `admin_password` varchar(255) NOT NULL,
  PRIMARY KEY (`admin_username`),
  UNIQUE KEY `admin_username` (`admin_username`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `admin`
--

LOCK TABLES `admin` WRITE;
/*!40000 ALTER TABLE `admin` DISABLE KEYS */;
INSERT INTO `admin` VALUES ('admin','admin');
/*!40000 ALTER TABLE `admin` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `complaints`
--

DROP TABLE IF EXISTS `complaints`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `complaints` (
  `complaint_id` int NOT NULL AUTO_INCREMENT,
  `resident` varchar(100) DEFAULT NULL,
  `category` varchar(50) NOT NULL,
  `status` enum('submitted','in progress','resolved','rejected') NOT NULL DEFAULT 'submitted',
  `date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `img` varchar(255) DEFAULT NULL,
  `address` varchar(255) NOT NULL,
  `details` text NOT NULL,
  PRIMARY KEY (`complaint_id`)
) ENGINE=MyISAM AUTO_INCREMENT=47 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `complaints`
--

LOCK TABLES `complaints` WRITE;
/*!40000 ALTER TABLE `complaints` DISABLE KEYS */;
INSERT INTO `complaints` VALUES (1,'Juan Dela Cruz','Garbage','in progress','2025-09-06 08:33:53','uploads/img1.jpg','123 Main St, Barangay Uno','Garbage has not been collected for 3 days.'),(2,'Maria Santos','Noise','rejected','2025-09-06 08:33:53',NULL,'45 Mabini St, Barangay Dos','Loud karaoke every night until 2am.'),(3,'Jose Ramirez','Road Damage','submitted','2025-09-06 08:33:53','uploads/img2.jpg','678 Rizal Ave, Barangay Tres','Huge pothole in front of our house.'),(4,'Ana Lopez','Water Supply','rejected','2025-09-06 08:33:53',NULL,'12 P. Burgos St, Barangay Cuatro','No running water since last week.'),(5,'Pedro Reyes','Streetlight','rejected','2025-09-06 08:33:53',NULL,'89 Del Pilar St, Barangay Cinco','Broken streetlight near the basketball court.'),(45,'Lyshael - Phone','Drainage / Flooding','submitted','2025-09-13 01:50:20',NULL,'Lower Quarry','No flood control'),(46,'Francie - iPad','Noise','submitted','2025-09-13 01:50:35',NULL,'Baguio','Too much noise'),(44,'Nicole - PC','Road','submitted','2025-09-13 01:50:20',NULL,'Bakakeng','Too many potholes'),(43,'Andrei Dela Cruz','Garbage','in progress','2025-09-13 01:19:29','CMP-43.jpg','Bakakeng Norte, Baguio City','Garbage pickup schedule isn\'t always one time so there\'s too many garbage lying around.');
/*!40000 ALTER TABLE `complaints` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Temporary view structure for view `complaints_view`
--

DROP TABLE IF EXISTS `complaints_view`;
/*!50001 DROP VIEW IF EXISTS `complaints_view`*/;
SET @saved_cs_client     = @@character_set_client;
/*!50503 SET character_set_client = utf8mb4 */;
/*!50001 CREATE VIEW `complaints_view` AS SELECT 
 1 AS `complaint_id`,
 1 AS `resident`,
 1 AS `category`,
 1 AS `status`,
 1 AS `filed_date`,
 1 AS `address`,
 1 AS `details`,
 1 AS `img`*/;
SET character_set_client = @saved_cs_client;

--
-- Final view structure for view `complaints_view`
--

/*!50001 DROP VIEW IF EXISTS `complaints_view`*/;
/*!50001 SET @saved_cs_client          = @@character_set_client */;
/*!50001 SET @saved_cs_results         = @@character_set_results */;
/*!50001 SET @saved_col_connection     = @@collation_connection */;
/*!50001 SET character_set_client      = utf8mb3 */;
/*!50001 SET character_set_results     = utf8mb3 */;
/*!50001 SET collation_connection      = utf8mb3_general_ci */;
/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=`root`@`localhost` SQL SECURITY DEFINER */
/*!50001 VIEW `complaints_view` AS select concat('CMP-',lpad(`complaints`.`complaint_id`,4,'0')) AS `complaint_id`,`complaints`.`resident` AS `resident`,`complaints`.`category` AS `category`,`complaints`.`status` AS `status`,date_format(`complaints`.`date`,'%Y-%m-%d %H:%i:%s') AS `filed_date`,`complaints`.`address` AS `address`,`complaints`.`details` AS `details`,`complaints`.`img` AS `img` from `complaints` */;
/*!50001 SET character_set_client      = @saved_cs_client */;
/*!50001 SET character_set_results     = @saved_cs_results */;
/*!50001 SET collation_connection      = @saved_col_connection */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2025-09-13 14:36:10
