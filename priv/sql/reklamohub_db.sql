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
  `admin_id` int NOT NULL AUTO_INCREMENT,
  `admin_username` varchar(50) NOT NULL,
  `admin_password` varchar(255) NOT NULL,
  PRIMARY KEY (`admin_id`),
  UNIQUE KEY `admin_username` (`admin_username`)
) ENGINE=MyISAM AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `admin`
--

LOCK TABLES `admin` WRITE;
/*!40000 ALTER TABLE `admin` DISABLE KEYS */;
INSERT INTO `admin` VALUES (1,'admin','admin');
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
) ENGINE=MyISAM AUTO_INCREMENT=11 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `complaints`
--

LOCK TABLES `complaints` WRITE;
/*!40000 ALTER TABLE `complaints` DISABLE KEYS */;
INSERT INTO `complaints` VALUES (1,'Juan Dela Cruz','Garbage','submitted','2025-09-06 08:33:53','uploads/img1.jpg','123 Main St, Barangay Uno','Garbage has not been collected for 3 days.'),(2,'Maria Santos','Noise','in progress','2025-09-06 08:33:53',NULL,'45 Mabini St, Barangay Dos','Loud karaoke every night until 2am.'),(3,'Jose Ramirez','Road Damage','resolved','2025-09-06 08:33:53','uploads/img2.jpg','678 Rizal Ave, Barangay Tres','Huge pothole in front of our house.'),(4,'Ana Lopez','Water Supply','rejected','2025-09-06 08:33:53',NULL,'12 P. Burgos St, Barangay Cuatro','No running water since last week.'),(5,'Pedro Reyes','Streetlight','submitted','2025-09-06 08:33:53',NULL,'89 Del Pilar St, Barangay Cinco','Broken streetlight near the basketball court.'),(6,'Juan Dela Cruz','Garbage','submitted','2025-09-06 08:35:00','uploads/img1.jpg','123 Main St, Barangay Uno','Garbage has not been collected for 3 days.'),(7,'Maria Santos','Noise','in progress','2025-09-06 08:35:00',NULL,'45 Mabini St, Barangay Dos','Loud karaoke every night until 2am.'),(8,'Jose Ramirez','Road Damage','resolved','2025-09-06 08:35:00','uploads/img2.jpg','678 Rizal Ave, Barangay Tres','Huge pothole in front of our house.'),(9,'Ana Lopez','Water Supply','rejected','2025-09-06 08:35:00',NULL,'12 P. Burgos St, Barangay Cuatro','No running water since last week.'),(10,'Pedro Reyes','Streetlight','submitted','2025-09-06 08:35:00',NULL,'89 Del Pilar St, Barangay Cinco','Broken streetlight near the basketball court.');
/*!40000 ALTER TABLE `complaints` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2025-09-06 16:35:24
