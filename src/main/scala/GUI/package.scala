import scalafx.application.JFXApp3
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ComboBox, Label, ScrollPane, Tab, TabPane, TableColumn, TableView, TextArea}
import scalafx.scene.layout.{BorderPane, GridPane, TilePane, VBox}
import scalafx.scene.paint.Color
import scalafx.Includes._
import scalafx.stage.{DirectoryChooser, FileChooser, Modality, Screen, Stage}
import scalafx.geometry.{Insets, Pos, Side}
import rocFB.{Asignacion, Estudiante, Materias, rocFB}
import rocPD.rocPD
import FileIO.{readFile, writeFile}
import scalafx.beans.property.{ObjectProperty, StringProperty}
import scalafx.collections.ObservableBuffer
import scalafx.scene.paint.Color._
import scalafx.scene.paint._
import scalafx.scene.text.Text
import scalafx.stage.Stage.sfxStage2jfx



object GUI extends JFXApp3 {

  def visualizationMenu(e:(Double, Double, Materias, Vector[Estudiante])):Unit = {

    //val screenBounds = Screen.primary.visualBounds
    case class SubjectsQuota(code: Int, quota: Int)

    val dataSQ = ObservableBuffer[SubjectsQuota]()

    for (m <- e._3) {
      dataSQ += SubjectsQuota(m._1, m._2)
    }


    val subjectsQuotaTable = new TableView(dataSQ)

    val subjectsQuotaTableCol1 = new TableColumn[SubjectsQuota, Int]("Codigo de Materia")
    subjectsQuotaTableCol1.cellValueFactory = cdf => ObjectProperty(cdf.value.code);
    subjectsQuotaTableCol1.prefWidth = 200.0

    val subjectsQuotaTableCol2 = new TableColumn[SubjectsQuota, Int]("Cupos")
    subjectsQuotaTableCol2.cellValueFactory = cdf => ObjectProperty(cdf.value.quota);
    subjectsQuotaTableCol2.prefWidth = 150.0

    subjectsQuotaTable.columns ++= List(subjectsQuotaTableCol1, subjectsQuotaTableCol2)

    val visualizeInput = new Stage {
      title = "Visualizar Entrada"
      scene = new Scene  {
        val tabPane = new TabPane {

          tabClosingPolicy = TabPane.TabClosingPolicy.Unavailable

          val subjectsQuotaTab = new Tab {
            text = "Materias"
            content = subjectsQuotaTable

          }

          tabs = Seq(subjectsQuotaTab)

          case class SubjectsPriorities(code: Int, priority: Int)

          for (i <- e._4) {


            val dataSP = ObservableBuffer[SubjectsPriorities]()

            for (j <- i._2) {
              dataSP += SubjectsPriorities(j._1, j._2)
            }

            val subjectsPrioritiesTable = new TableView(dataSP)

            val subjectsPrioritiesTableCol1 = new TableColumn[SubjectsPriorities, Int]("Codigo de Materia")
            subjectsPrioritiesTableCol1.cellValueFactory = cdf => ObjectProperty(cdf.value.code);
            subjectsPrioritiesTableCol1.prefWidth = 200.0

            val subjectsPrioritiesTableCol2 = new TableColumn[SubjectsPriorities, Int]("Prioridades Asignadas")
            subjectsPrioritiesTableCol2.cellValueFactory = cdf => ObjectProperty(cdf.value.priority);
            subjectsPrioritiesTableCol2.prefWidth = 200.0

            subjectsPrioritiesTable.columns ++= List(subjectsPrioritiesTableCol1, subjectsPrioritiesTableCol2)


            val tab = new Tab {
              text = "Estudiante " + i._1
              content = subjectsPrioritiesTable
            }


            tabs += tab
          }

        }

        val sp = new ScrollPane()
        sp.setMaxHeight(400)
        sp.setMaxWidth(600)


        sp.content = tabPane

        content = sp
      }
    }

    visualizeInput.initModality(Modality.ApplicationModal)
    visualizeInput.showAndWait();

  }

  def visualizationMenuOutput(e:(Asignacion, Double)): Unit = {
    val visualizeOutput = new Stage {
      title = "Visualizar Salida"
      scene = new Scene {
        val tabPane = new TabPane {

          tabs = Seq[Tab]()

          case class SubjectsPriorities(code: Int, priority: Int)

          for (i <- e._1) {

            val dataSP = ObservableBuffer[SubjectsPriorities]()

            for (j <- i._2) {
              dataSP += SubjectsPriorities(j._1, j._2)
            }

            val subjectsPrioritiesTable = new TableView(dataSP)

            val subjectsPrioritiesTableCol1 = new TableColumn[SubjectsPriorities, Int]("Codigo de Materia")
            subjectsPrioritiesTableCol1.cellValueFactory = cdf => ObjectProperty(cdf.value.code);
            subjectsPrioritiesTableCol1.prefWidth = 200.0

            val subjectsPrioritiesTableCol2 = new TableColumn[SubjectsPriorities, Int]("Prioridades Asignadas")
            subjectsPrioritiesTableCol2.cellValueFactory = cdf => ObjectProperty(cdf.value.priority);
            subjectsPrioritiesTableCol2.prefWidth = 200.0


            subjectsPrioritiesTable.columns ++= List(subjectsPrioritiesTableCol1, subjectsPrioritiesTableCol2)


            tabs += new Tab {
              text = "Estudiante " + i._1
              content = subjectsPrioritiesTable
            }
          }

        }

        val sp = new ScrollPane()

        sp.setMaxHeight(400)
        sp.setMaxWidth(600)

        sp.content = tabPane

        content = sp
      }
    }
    visualizeOutput.initModality(Modality.ApplicationModal)
    visualizeOutput.showAndWait();
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "REPARTICION OPTIMA DE CUPOS"

      scene = new Scene(width = 400,height = 600) {
        fill = Color.rgb(236, 215, 198)

      resizable = false;

      val txt = new Text {
        text = "ROC"
        style = "-fx-font: normal bold 50pt sans-serif"
        fill = new LinearGradient(
          endX = 0,
          stops = Stops(Red, DarkRed))
      }

        val algorithmHeader = new Text {
          text = "Tipo de Algoritmo"
          style = "-fx-font: bold 10pt sans-serif"
        }
        val algorithmTypeLabel = new Label("Seleccione el tipo de algoritmo");
        val comboBoxItems = List("Fuerza Bruta", "Voraz", "Dinamico");
        val algorithmTypeComboBox = new ComboBox[String](comboBoxItems);
        algorithmTypeComboBox.selectionModel().select(0);


        val inputHeader = new Text {
          text = "Entrada"
          style = "-fx-font: bold 10pt sans-serif"
        }
        val inputFileLabel = new Label("Seleccione un archivo de entrada");
        val inputFileButton = new Button("Seleccionar");
        val selectedInputFileLabel = new Label("Ninguno");
        val inputVisualizeButton = new Button("Visualizar");

        inputVisualizeButton.disable = true


        val outputHeader = new Text {
          text = "Salida"
          style = "-fx-font: bold 10pt sans-serif"
        }
        val outputDirLabel = new Label("Directorio de salidas");
        val outputDirButton = new Button("Seleccionar");

        val executionHeader = new Text {
          text = "Ejecución"
          style = "-fx-font: bold 10pt sans-serif"
        }
        val selectedOutputDirLabel = new Label("Ninguno");
        val executeProgramButton = new Button("Ejecutar");

        executeProgramButton.disable = true;

        val executionResultLabel = new Label("");

        val txtVBox = new VBox(5, txt);

        val strategyPickVBox = new VBox(5, algorithmHeader, algorithmTypeLabel, algorithmTypeComboBox);

        val inputVBox = new VBox(5, inputHeader, inputFileLabel, inputFileButton,
          selectedInputFileLabel, inputVisualizeButton);

        val outputVBox = new VBox(5, outputHeader, outputDirLabel, outputDirButton,
          selectedOutputDirLabel);

        val executionVBox = new VBox(5, executionHeader, executeProgramButton, executionResultLabel);


        inputFileButton.onAction = (e: ActionEvent) => {
          val fileChooser = new FileChooser;
          var selectedFile = fileChooser.showOpenDialog(stage) + "";
          if (selectedFile == "null") {
            selectedFile = "Ninguno";
            executeProgramButton.disable = true;
            inputVisualizeButton.disable = true
          } else {
            val outputPath = selectedOutputDirLabel.text();
            inputVisualizeButton.disable = false;
            if (outputPath != "Ninguno") {
              executeProgramButton.disable = false;
            }
          };

          selectedInputFileLabel.text = selectedFile;

        }


        inputVisualizeButton.onAction = (e: ActionEvent) => {

          try {
            val inputPath = selectedInputFileLabel.text();

            val e = readFile(inputPath)

            visualizationMenu(e)
          } catch {
            case _ => executionResultLabel.text = "Ha ocurrido un error";
          }
        }



        outputDirButton.onAction = (e:ActionEvent) => {
          val dirChooser = new DirectoryChooser;
          var selectedFile = dirChooser.showDialog(stage)+"";
          if (selectedFile == "null") {
            selectedFile = "Ninguno";
            executeProgramButton.disable = true;
          } else {
            val inputPath = selectedInputFileLabel.text();
            if (inputPath != "Ninguno") executeProgramButton.disable = false;
          }

          selectedOutputDirLabel.text = selectedFile;
        }


        executeProgramButton.onAction = (e: ActionEvent) => {

          try {

            val selectedStrategy = algorithmTypeComboBox.selectionModel.apply.getSelectedIndex();

            val inputPath = selectedInputFileLabel.text();

            val e = readFile(inputPath)

            val fileName = inputPath.split("/").last;

            var strategyPrefix = "";

            val sol = selectedStrategy match {
              case 0 => {strategyPrefix="fb";rocFB(e._1,e._2,e._3,e._4)}
              case 2 => {strategyPrefix="pd";rocPD(e._1,e._2,e._3,e._4)}
              case _ => {strategyPrefix="pd";rocPD(e._1,e._2,e._3,e._4)} // Placeholder
            }

            visualizationMenuOutput(sol)

            executionResultLabel.text = "Procesando...";

            val outputPath = selectedOutputDirLabel.text()

            val finalName = strategyPrefix + "_" + fileName

            writeFile(sol, outputPath, finalName);

            executionResultLabel.text = "Ejecutado exitosamente";

          } catch {
            case _ => executionResultLabel.text = "Ha ocurrido un error";
          }

        }
/*
        txtVBox.layoutX = 80;
        txtVBox.layoutY = 20;

        strategyPickVBox.layoutX = 80;
        strategyPickVBox.layoutY = 110;

        inputVBox.layoutX = 80;
        inputVBox.layoutY = 200;

        outputVBox.layoutX = 80;
        outputVBox.layoutY = 300;

        executionVBox.layoutX = 80;
        executionVBox.layoutY = 420;*/


        val mainVBox = new VBox (30, txtVBox, strategyPickVBox, inputVBox, outputVBox, executionVBox)


        mainVBox.layoutX = 90
        mainVBox.layoutY = 20

        content = mainVBox

      }
    }

  }
}