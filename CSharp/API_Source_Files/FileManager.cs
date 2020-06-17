using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleFileManager
{
    class FileManager
    {
    }
    public class SimpleFileCopy
    {
        public void CopyMain(string scenario)
        {
            //string fileName = "LULC_Scenario.txt";
            string fileName = "LCLU_Scenario.txt";
            // pause
            System.Threading.Thread.Sleep(3000);
            //
//            string sourcePath = @"C:\WaterSim\CSharp\API_10_0\API_Projects\API_WaterSim_6\ModelTesting\bin\Debug\App_Data\WaterSim_6_0\App_Data\Data\backupLULC\" + scenario;
  //          string sourcePath = @"C:\WaterSim\CSharp\API_10_0\API_Projects\API_WaterSim_6p5\API_WaterSim\bin\Debug\App_Data\WaterSim_6_0\App_Data\Data\NewLCLU\" + scenario;
            string sourcePath = @"C:\WaterSim\CSharp\API_11_1\API_Projects\WaterSim_6\API_WaterSim_6\API_WaterSim\bin\Debug\App_Data\WaterSim_6_0\App_Data\Data\NewLCLU\" + scenario;

//            string targetPath = @"C:\WaterSim\CSharp\API_10_0\API_Projects\API_WaterSim_6p5\API_WaterSim\bin\Debug\App_Data\WaterSim_6_0\App_Data\Data";
            string targetPath = @"C:\WaterSim\CSharp\API_11_1\API_Projects\WaterSim_6\API_WaterSim_6\API_WaterSim\bin\Debug\App_Data\WaterSim_6_0\App_Data\Data";

            // Use Path class to manipulate file and directory paths.
            string sourceFile = System.IO.Path.Combine(sourcePath, fileName);
            string destFile = System.IO.Path.Combine(targetPath, fileName);

            // To copy a folder's contents to a new location:
            // Create a new target folder, if necessary.
            if (!System.IO.Directory.Exists(targetPath))
            {
                System.IO.Directory.CreateDirectory(targetPath);
            }


            // To copy a file to another location and 
            // overwrite the destination file if it already exists.
            if (System.IO.File.Exists(sourceFile))
            {
                System.IO.File.Copy(sourceFile, destFile, true);
                Console.WriteLine("Successful Copy.");
            }
            else
            {
                Console.WriteLine("Source file does not exist!");
                System.Threading.Thread.Sleep(700);
            }
            // Keep console window open in debug mode.
            //Console.WriteLine("Successful.");
            System.Threading.Thread.Sleep(2000);
            //Console.ReadKey();
        }


        public void copyScenarioFile(int scenario)
        {
            SimpleFileCopy myFile = new SimpleFileCopy();
            // string[] sScenario = new string[FScenario] { "AD", "AF", "AH", "HHH", "EC", "ZW", "BAU" };

            string path = @"BAU\";
            switch (scenario)
            {
                case 1:
                    path = @"AD\";
                    break;
                case 2:
                    path = @"AF\";
                    break;
                case 3:
                    path = @"AH\";
                    break;
                case 4:
                    path = @"HHH\";
                    break;
                case 5:
                    path = @"EC\";
                    break;
                case 6:
                    path = @"ZW\";
                    break;
                case 7:
                    path = @"BAU\";
                    break;
                case 8:
                    path = @"BAU\";
 
                    break;
                case 9:
                    path = @"ZW\";

                    break;

                default:
                    path = @"BAU\";

                    break;
            }

            myFile.CopyMain(path);

        }

    }
    // Simple synchronous file deletion operations with no user interface.
    // To run this sample, create the following files on your drive:
    // C:\Users\Public\DeleteTest\test1.txt
    // C:\Users\Public\DeleteTest\test2.txt
    // C:\Users\Public\DeleteTest\SubDir\test2.txt
 
    public class SimpleFileDelete
    {
        static void DeleteMain()
        {
            // Delete a file by using File class static method...
            if (System.IO.File.Exists(@"C:\Users\Public\DeleteTest\test.txt"))
            {
                // Use a try block to catch IOExceptions, to
                // handle the case of the file already being
                // opened by another process.
                try
                {
                    System.IO.File.Delete(@"C:\Users\Public\DeleteTest\test.txt");
                }
                catch (System.IO.IOException e)
                {
                    Console.WriteLine(e.Message);
                    return;
                }
            }

            // ...or by using FileInfo instance method.
            System.IO.FileInfo fi = new System.IO.FileInfo(@"C:\Users\Public\DeleteTest\test2.txt");
            try
            {
                fi.Delete();
            }
            catch (System.IO.IOException e)
            {
                Console.WriteLine(e.Message);
            }

            // Delete a directory. Must be writable or empty.
            try
            {
                System.IO.Directory.Delete(@"C:\Users\Public\DeleteTest");
            }
            catch (System.IO.IOException e)
            {
                Console.WriteLine(e.Message);
            }
            // Delete a directory and all subdirectories with Directory static method...
            if (System.IO.Directory.Exists(@"C:\Users\Public\DeleteTest"))
            {
                try
                {
                    System.IO.Directory.Delete(@"C:\Users\Public\DeleteTest", true);
                }

                catch (System.IO.IOException e)
                {
                    Console.WriteLine(e.Message);
                }
            }

            // ...or with DirectoryInfo instance method.
            System.IO.DirectoryInfo di = new System.IO.DirectoryInfo(@"C:\Users\Public\public");
            // Delete this dir and all subdirs.
            try
            {
                di.Delete(true);
            }
            catch (System.IO.IOException e)
            {
                Console.WriteLine(e.Message);
            }

        }
    }
    // Simple synchronous file move operations with no user interface.
    public class SimpleFileMove
    {
        static void MoveMain()
        {
            string sourceFile = @"C:\Users\Public\public\test.txt";
            string destinationFile = @"C:\Users\Public\private\test.txt";

            // To move a file or folder to a new location:
            System.IO.File.Move(sourceFile, destinationFile);

            // To move an entire directory. To programmatically modify or combine
            // path strings, use the System.IO.Path class.
            System.IO.Directory.Move(@"C:\Users\Public\public\test\", @"C:\Users\Public\private");
        }
    }

}
