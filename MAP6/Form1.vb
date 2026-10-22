Imports System.Data.Common
Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports System.IO
Imports System.Net
Imports System.Reflection
Imports System.Runtime.Intrinsics
Imports System.Windows.Forms
Imports System.Windows.Forms.VisualStyles
'Imports System.Windows.Forms.VisualStyles.VisualStyleElement
Imports Windows.Win32.System

Public Class Form1
    '############################################################### Block Arc ###############################################################
    Public blockArcs As New List(Of BlockArc) ' List to store all block arcs
    Public isBlockArcDragging As Boolean = False
    Public offsetX_BlockArc As Integer
    Public offsetY_BlockArc As Integer
    Public isPERCENTILETABlockArcVisible As Boolean = False
    Public isAverageTABlockArcVisible As Boolean = False
    '#########################################################################################################################################
    Public SelectedGradientColorIndex As Integer = 0
    Public CELLDATA(,) As String ' Array to store parsed file content
    Public TA_Location(100000, 60, 8) As Double
    'Public TA_Location_Cartesian(100000, 55, 2) As Double
    Public TA_DATA(100000, 60, 0) As String
    Public TADATA As New List(Of String()) ' To store the parsed data
    Public CellCount As Integer
    Public CLength As Double = 4 / 4 ' 2
    Public CDelta As Double = 8 / 3 / 4 '4
    Public OLCEK As Double = 25
    Public BKY As Double
    Public BKX As Double
    Public BPY As Single = 200
    Public BPX As Single = 257
    Public SelectedCellCount As Integer
    Public SelectedCellContainer As String
    Public ResetCount As Integer = 0
    Public ParameterCount As Integer
    Public ParameterString As String
    Public Parameters As List(Of String)
    Public ShowLabels As Boolean = False
    Public LabelLocations As New List(Of Point)
    Public Label_offsetX As Double
    Public Label_offsetY As Double
    '####################################################### CIRCLE/RECTANGULAR/POLYGON DRAWING FOR SELECTION #######################################################
    Private Enum DrawingMode
        None
        Line
        Rectangle
        Circle
        Polygon
    End Enum

    Public Polygon_Coordinates(10000, 5) As Double
    Public Polygon_Lines(10000, 4) As Double
    Private currentMode As DrawingMode = DrawingMode.None
    Private isPolygonDrawing As Boolean = False
    Private startPoint As Point? = Nothing
    Private vertices As New List(Of Point) ' Stores vertices of the polygon
    Private movingVertexIndex As Integer = -1 ' Index of the vertex being dragged
    Private freeHandPoints As New List(Of Point) ' Stores points for free-hand drawing
    Private isPolygonComplete As Boolean = False ' Indicates if the polygon is complete

    ' Parameters to control the number of circles and their size
    Private Const pointDistanceThreshold As Integer = 20 ' Minimum distance between points to be considered a vertex
    Private Const circleRadius As Integer = 10 ' Radius of the circles
    Private Shape_Start_X As Double
    Private Shape_Start_Y As Double
    Private Shape_Start_LON As Double
    Private Shape_Start_LAT As Double
    Private Shape_End_X As Double
    Private Shape_End_Y As Double
    Private Shape_End_LON As Double
    Private Shape_End_LAT As Double
    Private Shape_Length As Double
    '##############################################################################################################################################################

    '####################################################### BUTTONS ##############################################################################################
    Dim M_Pointer As Boolean = True
    Dim M_Hold As Boolean = False
    Dim M_Polygon As Boolean = False
    Dim M_Rectangle As Boolean = False
    Dim M_Circle As Boolean = False
    Dim M_Measure As Boolean = False
    Dim M_Network_Loaded As Boolean = False
    '##############################################################################################################################################################


    Private is_NE_Dragging As Boolean = False
    Public selectedCELLIndex As Integer = -1
    Private previousMousePosition As PointF ' Track previous mouse position
    Private tooltip As ToolTip ' Tooltip object to show information
    Private tooltipTA As ToolTip ' Tooltip object to show information
    Private currentTooltipIndex As Integer = -1 ' Track the currently hovered arrow to prevent flickering
    Private currentTooltipIndexTA As Integer = -1 ' Track the currently hovered arrow to prevent flickering

    '####################################################### PARAMETERS FROM FILE #################################################################################
    Public latclmn As Integer
    Public lonclmn As Integer
    Public aziclmn As Integer
    Public earfcndlclmn As Integer
    Public eutrancellfddidclmn As Integer
    Public clmncount As Integer
    Public eutrancellrelationidclmn As Integer
    Public physicallayercellidclmn As Integer
    Public rachrootsequenceclmn As Integer
    Public tacclmn As Integer
    Public ecgilmn As Integer
    Public cellrangeclmn As Integer
    '##############################################################################################################################################################

    Public RelationLinestartPoint As Point
    Public RelationLineendPoint As Point


    Public Sub FileImport()
        'Dim a As Integer
        ' File path
        Dim filePath As String = "C:\NETEX\ERICSSONDATA\EXPORTS\SUBS\ENODEB_SUBS\ENODEB_CELLDATA3.txt"
        Dim TotalLATITUDE As Double
        Dim TotalLONGITUDE As Double

        ' Read all lines of the file
        Dim lines() As String = File.ReadAllLines(filePath)

        ' Find the number of columns by splitting the first line (header)
        Dim headerColumns() As String = lines(0).Split("#"c)
        Dim numberOfColumns As Integer = headerColumns.Length

        ' Initialize 2D array to store data for each row and column
        ReDim CELLDATA(lines.Length, numberOfColumns)
        'MsgBox(lines.Length)
        ' Parse the file and store all data in the array
        For i As Integer = 0 To lines.Length - 1
            Dim columns() As String = lines(i).Split("#"c)

            For j As Integer = 0 To columns.Length - 1
                CELLDATA(i, j) = columns(j).Trim()
            Next
            'a = i
        Next
        'MsgBox(lines(lines.Length - 1))
        'MsgBox(lines.Length - 1)
        ' Example of accessing parsed data
        'MessageBox.Show("First entry - First Column: " & CELLDATA(0, 0) & " Last Column: " & CELLDATA(2, numberOfColumns - 1))
        'MessageBox.Show(lines.Length)
        'MessageBox.Show(headerColumns.Length)
        CellCount = lines.Length - 1
        ParameterCount = numberOfColumns
        ParameterString = ""
        For j = 0 To numberOfColumns
            ParameterString = ParameterString & Trim(CELLDATA(0, j)) & "#"
            If InStr(UCase(Trim(CELLDATA(0, j))), "LATITUDE") Then latclmn = j
            If InStr(UCase(Trim(CELLDATA(0, j))), "LONGITUDE") Then lonclmn = j
            If InStr(UCase(Trim(CELLDATA(0, j))), "AZIMUTH") Then aziclmn = j
            If InStr(UCase(Trim(CELLDATA(0, j))), "EARFCNDL") Then earfcndlclmn = j
            If InStr(UCase(Trim(CELLDATA(0, j))), "EUTRANCELLFDDID") Then eutrancellfddidclmn = j
            If (UCase(Trim(CELLDATA(0, j))) = UCase(Trim("EUTRANCELLRELATIONID"))) Then eutrancellrelationidclmn = j
            If (UCase(Trim(CELLDATA(0, j))) = "PHYSICALLAYERCELLID") Then physicallayercellidclmn = j
            If (UCase(Trim(CELLDATA(0, j))) = "RACHROOTSEQUENCE") Then rachrootsequenceclmn = j
            If (UCase(Trim(CELLDATA(0, j))) = "TAC") Then tacclmn = j
            If (UCase(Trim(CELLDATA(0, j))) = "CELLRANGE") Then cellrangeclmn = j
            'cellrangeclmn
        Next

        For k = 1 To CellCount
            TotalLATITUDE = TotalLATITUDE + CDbl(CELLDATA(k, latclmn))
            TotalLONGITUDE = TotalLONGITUDE + CDbl(CELLDATA(k, lonclmn))
        Next k

        BKY = CDbl(TotalLATITUDE / CellCount)
        BKX = CDbl(TotalLONGITUDE / CellCount)

    End Sub
    Public Sub LoadNetwork()
        FileImport()
        ReDim CELLS(CellCount - 1)
        LoadCELLS()
        Combo1Parameters()
        Combo3Parameters()
        ListBox1Parameters()
    End Sub
    Public Sub Combo1Parameters()
        Form2.ComboBox1O.Items.Clear()
        For i = 0 To ParameterCount
            Form2.ComboBox1O.Items.Add(Trim(CELLDATA(0, i)))
        Next
    End Sub
    Public Sub Combo3Parameters()
        Form2.ComboBox3.Items.Clear()
        For i = 0 To ParameterCount
            Form2.ComboBox3.Items.Add(Trim(CELLDATA(0, i)))
        Next
    End Sub
    Public Sub ListBox1Parameters()
        Dim filterText As String = Form2.LabelTextBox.Text
        Parameters = ParameterString.Split("#"c).ToList()
        Form2.LabelListBox.Items.Clear()
        For Each param As String In Parameters
            If param.Contains(filterText, StringComparison.OrdinalIgnoreCase) Then
                Form2.LabelListBox.Items.Add(param)
            End If
        Next
    End Sub
    Public Sub LoadCELLS()
        Dim Calc1, Calc2, Calc3, Lat_r, Lon_r As Double
        Dim rnd As New Random()


        For i As Integer = 0 To CELLS.Length - 1  '''''''''1
            Dim sx As Single = rnd.Next(50, Me.ClientSize.Width - 50)
            Dim sy As Single = rnd.Next(50, Me.ClientSize.Height - 50)

            CELLS(i).pos = New PointF(7) {} ' 8 points for each CELL shape
            'CELLS(i).FillColor = Brushes.Green ' Initially fill with blue color
            CELLS(i).FillColor = New SolidBrush(Color.FromArgb(0, 255, 0))
            CELLS(i).BorderColor = Pens.Black ' Initially, the border is black with a default thickness
            CELLS(i).IsSelected = False ' Initially, none of the CELLS are selected
            'CELLS(i).TooltipText = "Arrow #" & i ' Set tooltip text for each arrow
            CELLS(i).Azimuth = CELLDATA(i + 1, aziclmn)
            CELLS(i).Latitude = CELLDATA(i + 1, latclmn)
            CELLS(i).Longitude = CELLDATA(i + 1, lonclmn)
            'CELLS(i).CartesianY = CELLDATA(i + 1, aziclmn)
            'CELLS(i).CartesianX = CELLDATA(i + 1, aziclmn)
            CELLS(i).CellName = CELLDATA(i + 1, eutrancellfddidclmn)
            CELLS(i).PCI = CELLDATA(i + 1, physicallayercellidclmn)
            CELLS(i).RSI = CELLDATA(i + 1, rachrootsequenceclmn)
            CELLS(i).TAC = CELLDATA(i + 1, tacclmn)
            CELLS(i).TooltipText = CELLDATA(i + 1, eutrancellfddidclmn)
            CELLS(i).Frequency = CELLDATA(i + 1, earfcndlclmn)
            CELLS(i).CELLRANGE = CELLDATA(i + 1, cellrangeclmn)
            CELLS(i).ECGI = ""
            CELLS(i).Index = i

            Lat_r = CELLS(i).Latitude * Math.PI / 180
            Lon_r = CELLS(i).Longitude * Math.PI / 180

            Calc1 = Math.Sin(Lat_r) * Math.Sin(Lat_r) + Math.Cos(Lat_r) * Math.Cos(Lat_r) * Math.Cos(Lon_r - (CELLS(i).Longitude - 1) * Math.PI / 180)
            'Calc1 = Math.Sin((CELLS(i).Latitude) * Math.PI / 180) * Math.Sin(CELLS(i).Latitude * Math.PI / 180) + Math.Cos(CELLS(i).Latitude * Math.PI / 180) * Math.Cos((CELLS(i).Latitude) * Math.PI / 180) * Math.Cos(CELLS(i).Longitude * Math.PI / 180 - (CELLS(i).Longitude - 1) * Math.PI / 180)
            Calc2 = Math.Atan(-Calc1 / Math.Sqrt(-Calc1 * Calc1 + 1)) + 2 * Math.Atan(1)
            'Calculate calc3 (the distance in meters)
            Calc3 = Math.Round(Calc2 * 6371, 6) * 1000
            'Calc3 = Math.Round(Math.Acos(Calc1) * 6371, 2) * 1000

            ' Calculate Cartesian X (map x)
            CELLS(i).CartesianX = ((CELLS(i).Longitude - BKX) * Calc3 * OLCEK + BPX * 1000) / 1000

            ' Calculate Cartesian Y (map y)
            CELLS(i).CartesianY = (BPY * 1000 - (CELLS(i).Latitude - BKY) * (111000 * OLCEK)) / 1000

            'If i < 20 Then TextBox1.Text = TextBox1.Text & CELLS(i).Latitude & "#" & CELLS(i).Longitude & "#" & Calc1 & "#" & Calc2 & "#" & Calc3 & "#" & CELLS(i).CartesianX & "#" & CELLS(i).CartesianY & "#" & BKX & "#" & BPX & "#" & BKY & "#" & BPY & "#" & OLCEK & vbCrLf

            GenerateShape(CELLS(i), CELLS(i).CartesianX, CELLS(i).CartesianY, CELLS(i).Azimuth, 33, CLength, CDelta, CELLS(i).Frequency) ' Custom shape generation

            'TA_Location(100000, 55, 2)
            'Dim lat2 As Double = Asin(Sin(lat1) * Cos(angularDistance) + Cos(lat1) * Sin(angularDistance) * Cos(theta))

        Next
        'TALOC()
        M_Network_Loaded = True
    End Sub
    ' CELL data structure to hold positions for each CELL
    Public Sub TALOC()
        Dim angularDistance, Lat_r, Lon_r, Azi_r As Double
        Dim EricssonTAIndex As Double() = {
            0.08, 0.23, 0.63, 1.01, 1.64, 2.42, 3.36, 4.45, 5.7, 7.18, 8.74, 10.5,
            12.5, 14.6, 16.9, 19.4, 22, 24.8, 27.9, 31.1, 34.5, 38.1, 41.8, 45.8,
            50, 54.3, 58.9, 63.5, 68.5, 73.6, 78.9, 84.5, 90.2, 96.1, 100
        }
        'MsgBox(UBound(values))

        For i As Integer = 0 To CELLS.Length - 1
            Lat_r = CELLS(i).Latitude * Math.PI / 180
            Lon_r = CELLS(i).Longitude * Math.PI / 180
            Azi_r = CELLS(i).Azimuth * Math.PI / 180

            For indx = 0 To UBound(EricssonTAIndex)
                angularDistance = EricssonTAIndex(indx) / 6371
                TA_Location(i, indx, 1) = Math.Asin(Math.Sin(Lat_r) * Math.Cos(angularDistance) + Math.Cos(Lat_r) * Math.Sin(angularDistance) * Math.Cos(Azi_r))
                TA_Location(i, indx, 2) = Lon_r + Math.Atan2(Math.Sin(Azi_r) * Math.Sin(angularDistance) * Math.Cos(Lat_r),
                                  Math.Cos(angularDistance) - Math.Sin(Lat_r) * Math.Sin(TA_Location(i, indx, 1)))
                TA_Location(i, indx, 1) = TA_Location(i, indx, 1) * 180.0 / Math.PI
                TA_Location(i, indx, 2) = TA_Location(i, indx, 2) * 180.0 / Math.PI
            Next

        Next

        Dim EricssonTAIndex_Avg As Double() = {
            0.04, 0.155, 0.43, 0.82, 1.325, 2.03, 2.89, 3.905, 5.075, 6.44, 7.96, 9.62,
            11.5, 13.55, 15.75, 18.15, 20.7, 23.4, 26.35, 29.5, 32.8, 36.3, 39.95, 43.8,
            47.9, 52.15, 56.6, 61.2, 66, 71.05, 76.25, 81.7, 87.35, 93.15, 98.05
        }

        Dim IndexArray As Integer() = {
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
            12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 60
        }

        Dim TotalSample, TotalSampleDistance As Double

        For i As Integer = 0 To CELLS.Length - 1
            TotalSample = 0
            TotalSampleDistance = 0
            For k = 0 To 34
                If IsNumeric(TA_Location(i, k, 5)) Then
                    TotalSampleDistance = TotalSampleDistance + TA_Location(i, k, 5) * EricssonTAIndex_Avg(k)
                    TotalSample = TotalSample + TA_Location(i, k, 5)
                End If
            Next

            If TotalSample > 0 And TotalSampleDistance > 0 Then
                Lat_r = CELLS(i).Latitude * Math.PI / 180
                Lon_r = CELLS(i).Longitude * Math.PI / 180
                Azi_r = CELLS(i).Azimuth * Math.PI / 180
                CELLS(i).TA_Avg_Dist = (TotalSampleDistance / TotalSample)
                angularDistance = CELLS(i).TA_Avg_Dist / 6371
                For indx = 60 To 60
                    TA_Location(i, indx, 1) = Math.Asin(Math.Sin(Lat_r) * Math.Cos(angularDistance) + Math.Cos(Lat_r) * Math.Sin(angularDistance) * Math.Cos(Azi_r))
                    TA_Location(i, indx, 2) = Lon_r + Math.Atan2(Math.Sin(Azi_r) * Math.Sin(angularDistance) * Math.Cos(Lat_r),
                                      Math.Cos(angularDistance) - Math.Sin(Lat_r) * Math.Sin(TA_Location(i, indx, 1)))
                    TA_Location(i, indx, 1) = TA_Location(i, indx, 1) * 180.0 / Math.PI
                    TA_Location(i, indx, 2) = TA_Location(i, indx, 2) * 180.0 / Math.PI
                Next
            End If

        Next

    End Sub
    Public Sub TALOC_Cartesian()
        'TA_Location_Cartesian(100000, 55, 2)
        Dim Calc1, Calc2, Calc3, Lat_r, Lon_r As Double

        Dim IndexArray As Integer() = {
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
            12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 60
        }


        For i As Integer = 0 To CELLS.Length - 1
            If CELLS(i).IsSelected = True Then
                For j = 0 To 34
                    Lat_r = TA_Location(i, j, 1) * Math.PI / 180
                    Lon_r = TA_Location(i, j, 2) * Math.PI / 180
                    Calc1 = Math.Sin(Lat_r) * Math.Sin(Lat_r) + Math.Cos(Lat_r) * Math.Cos(Lat_r) * Math.Cos(1 * Math.PI / 180)
                    Calc2 = Math.Atan(-Calc1 / Math.Sqrt(-Calc1 * Calc1 + 1)) + 2 * Math.Atan(1)
                    Calc3 = Math.Round(Calc2 * 6371, 6) * 1000

                    Lat_r = TA_Location(i, j, 1)
                    Lon_r = TA_Location(i, j, 2)

                    ' Calculate Cartesian X (map x)
                    TA_Location(i, j, 4) = ((Lon_r - BKX) * Calc3 * OLCEK + BPX * 1000) / 1000

                    ' Calculate Cartesian Y (map y)
                    TA_Location(i, j, 3) = (BPY * 1000 - (Lat_r - BKY) * (111000 * OLCEK)) / 1000
                Next

                For j = 59 To 60
                    Lat_r = TA_Location(i, j, 1) * Math.PI / 180
                    Lon_r = TA_Location(i, j, 2) * Math.PI / 180
                    Calc1 = Math.Sin(Lat_r) * Math.Sin(Lat_r) + Math.Cos(Lat_r) * Math.Cos(Lat_r) * Math.Cos(1 * Math.PI / 180)
                    Calc2 = Math.Atan(-Calc1 / Math.Sqrt(-Calc1 * Calc1 + 1)) + 2 * Math.Atan(1)
                    Calc3 = Math.Round(Calc2 * 6371, 6) * 1000

                    Lat_r = TA_Location(i, j, 1)
                    Lon_r = TA_Location(i, j, 2)

                    ' Calculate Cartesian X (map x)
                    TA_Location(i, j, 4) = ((Lon_r - BKX) * Calc3 * OLCEK + BPX * 1000) / 1000

                    ' Calculate Cartesian Y (map y)
                    TA_Location(i, j, 3) = (BPY * 1000 - (Lat_r - BKY) * (111000 * OLCEK)) / 1000
                Next

            End If
        Next
    End Sub

    Public Sub TA_AVG()

        Dim EricssonTAIndex_Avg As Double() = {
            0.04, 0.155, 0.43, 0.82, 1.325, 2.03, 2.89, 3.905, 5.075, 6.44, 7.96, 9.62,
            11.5, 13.55, 15.75, 18.15, 20.7, 23.4, 26.35, 29.5, 32.8, 36.3, 39.95, 43.8,
            47.9, 52.15, 56.6, 61.2, 66, 71.05, 76.25, 81.7, 87.35, 93.15, 98.05
        }

        'Dim count As Integer
        Dim TotalSample, TotalSampleDistance As Double
        'Dim angularDistance, Lat_r, Lon_r, Azi_r As Double
        'Dim Calc1, Calc2, Calc3 As Double
        For i As Integer = 0 To CELLS.Length - 1
            TotalSample = 0
            For k = 0 To 34
                If IsNumeric(TA_Location(i, k, 5)) Then
                    TotalSampleDistance = TotalSampleDistance + TA_Location(i, k, 5) * EricssonTAIndex_Avg(k)
                    TotalSample = TotalSample + TA_Location(i, k, 5)
                End If
            Next
        Next

    End Sub

    Public Sub TA_XPERCENTILE()
        'Form2.TextBox1.Text = ""
        Dim Percent As Integer
        Dim XPercentileSample As Double
        Dim XPercentileIndex As Integer
        Dim XPercentileDistance As Double
        Dim tempvariableDistance, tempvariablesample As Double
        Dim angularDistance, Lat_r, Lon_r, Azi_r As Double
        Percent = Form2.NumericUpDown1.Value

        Dim EricssonTAIndex As Double() = {
            0.08, 0.23, 0.63, 1.01, 1.64, 2.42, 3.36, 4.45, 5.7, 7.18, 8.74, 10.5,
            12.5, 14.6, 16.9, 19.4, 22, 24.8, 27.9, 31.1, 34.5, 38.1, 41.8, 45.8,
            50, 54.3, 58.9, 63.5, 68.5, 73.6, 78.9, 84.5, 90.2, 96.1, 100
        }

        Dim EricssonTAIndex_Avg As Double() = {
            0.04, 0.155, 0.43, 0.82, 1.325, 2.03, 2.89, 3.905, 5.075, 6.44, 7.96, 9.62,
            11.5, 13.55, 15.75, 18.15, 20.7, 23.4, 26.35, 29.5, 32.8, 36.3, 39.95, 43.8,
            47.9, 52.15, 56.6, 61.2, 66, 71.05, 76.25, 81.7, 87.35, 93.15, 98.05
        }

        Dim count As Integer = -1

        Dim TotalSample, temp As Double

        For i As Integer = 0 To CELLS.Length - 1
            If CELLS(i).IsSelected = True Then
                count = -1
                tempvariableDistance = 0
                tempvariablesample = 0
                TotalSample = 0
                For k = 0 To 34
                    If IsNumeric(TA_Location(i, k, 5)) Then
                        'TotalSampleDistance = TotalSampleDistance + form1.TA_Location(i, k, 5) * EricssonTAIndex_Avg(i)
                        TotalSample = TotalSample + TA_Location(i, k, 5)
                    End If
                Next
                If TotalSample > 0 Then XPercentileSample = Percent * TotalSample / 100
                If XPercentileSample > 0 Then
                    temp = 0
                    For k = 0 To 34
                        temp = temp + TA_Location(i, k, 5)

                        If (temp / TotalSample) * 100 < Percent Then
                            count = count + 1
                            TA_Location(i, k, 7) = TA_Location(i, k, 5)
                            TA_Location(i, k, 8) = TA_Location(i, k, 5) / TotalSample * 100
                            tempvariableDistance = tempvariableDistance + TA_Location(i, k, 5) * EricssonTAIndex_Avg(k)
                            tempvariablesample = tempvariablesample + TA_Location(i, k, 5)
                            'Form2.TextBox1.Text = Form2.TextBox1.Text & "," & TA_Location(i, k, 8)
                        ElseIf (temp / TotalSample) * 100 >= Percent Then
                            XPercentileIndex = count + 1
                            tempvariableDistance = tempvariableDistance + (XPercentileSample - tempvariablesample) * EricssonTAIndex_Avg(k)
                            XPercentileDistance = tempvariableDistance / XPercentileSample
                            TA_Location(i, k, 7) = (XPercentileSample - tempvariablesample)
                            TA_Location(i, k, 8) = TA_Location(i, k, 7) / TotalSample * 100
                            'Form2.TextBox1.Text = Form2.TextBox1.Text & "," & TA_Location(i, k, 8)
                            GoTo skip1
                        End If
                    Next
skip1:
                    'Form2.TextBox1.Text = Form2.TextBox1.Text & "," & XPercentileSample & "," & TotalSample
                    Lat_r = CELLS(i).Latitude * Math.PI / 180
                    Lon_r = CELLS(i).Longitude * Math.PI / 180
                    Azi_r = CELLS(i).Azimuth * Math.PI / 180
                    'CELLS(i).TA_XPerc_Dist = XPercentileDistance
                    CELLS(i).TA_XPerc_Dist = EricssonTAIndex(XPercentileIndex)
                    CELLS(i).TA_XPerc_Dist_Index = XPercentileIndex
                    angularDistance = CELLS(i).TA_XPerc_Dist / 6371
                    For indx = 59 To 59
                        TA_Location(i, indx, 1) = Math.Asin(Math.Sin(Lat_r) * Math.Cos(angularDistance) + Math.Cos(Lat_r) * Math.Sin(angularDistance) * Math.Cos(Azi_r))
                        TA_Location(i, indx, 2) = Lon_r + Math.Atan2(Math.Sin(Azi_r) * Math.Sin(angularDistance) * Math.Cos(Lat_r),
                                          Math.Cos(angularDistance) - Math.Sin(Lat_r) * Math.Sin(TA_Location(i, indx, 1)))
                        TA_Location(i, indx, 1) = TA_Location(i, indx, 1) * 180.0 / Math.PI
                        TA_Location(i, indx, 2) = TA_Location(i, indx, 2) * 180.0 / Math.PI
                    Next
                End If

            End If
        Next

    End Sub

    Public Structure CELL
        Public pos() As PointF ' Holds 8 points for the shape
        Public IsSelected As Boolean ' To track whether the CELL is selected (red or blue)
        Public FillColor As Brush ' Color of the CELL
        Public BorderColor As Pen ' Border color of the arrow
        Public IsSelectable As Boolean
        Public IsVisible As Boolean
        Public Azimuth As Single
        Public Latitude As Double
        Public Longitude As Double
        Public CartesianY As Double
        Public CartesianX As Double
        Public TA_Avg_Dist As Double
        Public TA_XPerc_Dist As Double
        Public TA_XPerc_Dist_Index As Integer
        Public Frequency As Integer
        Public CellName As String
        Public TAC As String
        Public PCI As Integer
        Public RSI As Integer
        Public CELLRANGE As Integer
        Public Index As Integer
        Public TooltipText As String
        Public ECGI As String
        Public Label As String
    End Structure

    Public CELLS() As CELL ' Array to hold 1000 CELLS

    Public Sub CELL_POSITION(i As Integer)

        Dim Calc1, Calc2, Calc3, Lat_r, Lon_r As Double
        Lat_r = CELLS(i).Latitude * Math.PI / 180
        Lon_r = CELLS(i).Longitude * Math.PI / 180

        Calc1 = Math.Sin(Lat_r) * Math.Sin(Lat_r) + Math.Cos(Lat_r) * Math.Cos(Lat_r) * Math.Cos(Lon_r - (CELLS(i).Longitude - 1) * Math.PI / 180)
        Calc2 = Math.Atan(-Calc1 / Math.Sqrt(-Calc1 * Calc1 + 1)) + 2 * Math.Atan(1)
        Calc3 = Math.Round(Calc2 * 6371, 6) * 1000
        'Calc3 = Math.Round(Math.Acos(Calc1) * 6371, 2) * 1000
        CELLS(i).CartesianX = ((CELLS(i).Longitude - BKX) * Calc3 * OLCEK + BPX * 1000) / 1000
        CELLS(i).CartesianY = (BPY * 1000 - (CELLS(i).Latitude - BKY) * (111000 * OLCEK)) / 1000
    End Sub
    ' Custom shape generator (similar to your CELL logic)
    Private Sub GenerateShape(ByRef CELL As CELL, sx As Single, sy As Single, aci As Single, bwaci As Single, CLength As Single, CDelta As Single, Frequency As Integer)

        Dim Findex As Single = 1

        If Frequency = 550 Then Findex = 1
        If Frequency = 575 Then Findex = 2
        If Frequency = 1724 Then Findex = 4
        If Frequency = 1850 Then Findex = 3
        If Frequency = 2850 Then Findex = 5
        If Frequency = 3425 Then Findex = 6
        If Frequency = 6375 Then Findex = 7

        bwaci = 33
        Findex = Findex * 2
        Dim Size1 As Single = (CLength + 1.5F * CDelta * Findex)
        Dim Size2 As Single = (CLength + 1.5F * (CDelta * (Findex + 2)))

        CELL.pos(0) = New PointF(sx, sy)
        CELL.pos(1) = New PointF(sx + Size1 * Math.Sin(aci / 180 * Math.PI), sy - Size1 * Math.Cos(aci / 180 * Math.PI))
        CELL.pos(2) = New PointF(sx + Size1 * 0.75F * Math.Sin((aci + bwaci) / 180 * Math.PI), sy - Size1 * 0.75F * Math.Cos((aci + bwaci) / 180 * Math.PI))
        CELL.pos(3) = New PointF(sx + Size2 * 0.75F * Math.Sin((aci + bwaci) / 180 * Math.PI), sy - Size2 * 0.75F * Math.Cos((aci + bwaci) / 180 * Math.PI))
        CELL.pos(4) = New PointF(sx + Size2 * Math.Sin(aci / 180 * Math.PI), sy - Size2 * Math.Cos(aci / 180 * Math.PI))
        CELL.pos(5) = New PointF(sx + Size2 * 0.75F * Math.Sin((aci - bwaci) / 180 * Math.PI), sy - Size2 * 0.75F * Math.Cos((aci - bwaci) / 180 * Math.PI))
        CELL.pos(6) = New PointF(sx + Size1 * 0.75F * Math.Sin((aci - bwaci) / 180 * Math.PI), sy - Size1 * 0.75F * Math.Cos((aci - bwaci) / 180 * Math.PI))
        CELL.pos(7) = CELL.pos(1) ' Closing the shape
    End Sub
    Public Sub LabelPosition()
        Dim Findex As Integer
        Dim NX, NY, LBLX, LBLY As Long
        ShowLabels = False
        Me.Invalidate()

        For i As Integer = 0 To CELLS.Length - 1
            If CELLS(i).Frequency = 550 Then Findex = 1
            If CELLS(i).Frequency = 575 Then Findex = 2
            If CELLS(i).Frequency = 1724 Then Findex = 4
            If CELLS(i).Frequency = 1850 Then Findex = 3
            If CELLS(i).Frequency = 2850 Then Findex = 5
            If CELLS(i).Frequency = 3425 Then Findex = 6
            If CELLS(i).Frequency = 6375 Then Findex = 7

            'Findex = 7
            'Findex = Findex * 2
            Dim Size2 As Single = (CLength + 1.5F * (CDelta * (10 + 2)))
            ' CELL.pos(4) = New PointF(CELLS(i).pos(0).X + Size2 * Math.Sin(aci / 180 * Math.PI), CELLS(i).pos(0).Y - Size2 * Math.Cos(aci / 180 * Math.PI))
            NX = CELLS(i).pos(0).X + Size2 * Math.Sin(CELLS(i).Azimuth / 180 * Math.PI)
            NY = CELLS(i).pos(0).Y - Size2 * Math.Cos(CELLS(i).Azimuth / 180 * Math.PI)

            'NX = CLength + 1.5 * (CDelta * (11)) * Math.Sin(CELLS(i).Azimuth / 180 * 3.14) / 1000 + CELLS(i).pos(0).X
            'NY = CELLS(i).pos(0).Y - (CLength + 1.5 * (CDelta * (7))) * Math.Cos(CELLS(i).Azimuth / 180 * 3.14) / 1000

            'NX = CELLS(i).pos(0).X
            'NY = CELLS(i).pos(0).Y


            If CELLS(i).Azimuth >= 0 And CELLS(i).Azimuth <= 90 Then
                LBLX = 0 'LEN(PPP(Index).Tooltiptext)
                LBLY = -12 * (Findex)
            End If

            If CELLS(i).Azimuth > 90 And CELLS(i).Azimuth <= 180 Then
                LBLX = 0
                LBLY = 0 + 12 * (Findex)
            End If

            If CELLS(i).Azimuth > 180 And CELLS(i).Azimuth <= 270 Then
                LBLX = (-8) * Len(CELLS(i).Label)
                LBLY = 0 + 12 * (Findex)
            End If

            If CELLS(i).Azimuth > 270 And CELLS(i).Azimuth <= 359 Then
                LBLX = (-8) * Len(CELLS(i).Label)
                LBLY = -12 * (Findex)
            End If



            'LabelLocations(i) = New Point(CELLS(i).pos(4).X, CELLS(i).pos(4).Y)
            LabelLocations(i) = New Point((NX + CLng(LBLX)), NY + CLng(LBLY))


        Next
        ShowLabels = True
        Me.Invalidate()
        Me.Refresh()
        'Distance.Text = LabelLocations.Count
    End Sub
    ' Override the paint event to draw the CELLS
    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        MyBase.OnPaint(e)
        Dim g As Graphics = e.Graphics
        Dim pen As New Pen(Color.Black)

        For Each CELL As CELL In CELLS
            ' Choose fill color based on whether the CELL is selected (red) or not (blue)
            If CELL.IsSelected Then
                'CELL.FillColor = Brushes.Red
                'CELL.FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
            Else
                'CELL.FillColor = New SolidBrush(Color.FromArgb(0, 255, 0))
            End If

            g.FillPolygon(CELL.FillColor, CELL.pos) ' Draw each CELL with its current fill color
            'g.DrawPolygon(Pens.Black, CELL.pos) ' Draw the outline of the CELL
            g.DrawPolygon(CELL.BorderColor, CELL.pos)
        Next

        If Form2.RelationLineVisible = True And Form2.RelationCount > 0 And SelectedCellCount = 1 And (Form2.OutgoingNeighborsCheckBox.Checked = True Or Form2.IncomingNeighborsCheckBox.Checked = True Or Form2.PCIConflictCheckBox.Checked = True Or Form2.PCIConfusionCheckBox.Checked = True Or Form2.RSIConflictCheckBox.Checked = True) Then
            Form2.RelationLineCoordinatesUpdate()
            For j = 1 To Form2.RelationCount
                RelationLinestartPoint = New Point(Form2.RelationCartesianCoordinates(j, 2), Form2.RelationCartesianCoordinates(j, 3))
                RelationLineendPoint = New Point(Form2.RelationCartesianCoordinates(j, 5), Form2.RelationCartesianCoordinates(j, 6))
                g.DrawLine(pen, RelationLinestartPoint, RelationLineendPoint)
            Next
        End If

    End Sub
    ' Initialize the form and draw the CELLS
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Enable double buffering to reduce flicker and improve performance
        Me.DoubleBuffered = True
        tooltip = New ToolTip() ' Initialize the tooltip
        tooltipTA = New ToolTip() ' Initialize the tooltip
        'Me.SetStyle(ControlStyles.AllPaintingInWmPaint Or ControlStyles.UserPaint Or ControlStyles.OptimizedDoubleBuffer, True)
        Me.UpdateStyles()

        LoadNetwork()

    End Sub


    ' Mouse down event to start dragging or detect clicks inside shapes
    Private Sub Form1_MouseDown(sender As Object, e As MouseEventArgs) Handles MyBase.MouseDown

        If M_Network_Loaded = True And (M_Pointer = True Or M_Hold = True) Then
            If e.Button = MouseButtons.Left Then
                If M_Pointer = True Then selectedCELLIndex = -1 ' Reset selection
                If M_Pointer = True Then DeSelectAll()
                For i As Integer = 0 To CELLS.Length - 1 ''''''''''''''''''''2

                    If IsPointInCell(e.Location, CELLS(i).pos) And M_Pointer = True Then
                        CELLS(i).IsSelected = True ' Mark the CELL as selected (red)
                        selectedCELLIndex = i ' Track the selected CELL
                        CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0)) ' Custom RGB color
                        CELLS(i).BorderColor = New Pen(Color.Black, 2)
                    End If

                    'If IsPointInCell(e.Location, CELLS(i).pos) = 0 And M_Pointer = True Then
                    'CELLS(i).FillColor = New SolidBrush(Color.FromArgb(0, 255, 0))  'Brushes.Blue ' Reset other arrows to blue
                    'CELLS(i).BorderColor = Pens.Black '
                    'CELL'S(i).IsSelected = False ' Reset other CELLS to unselected (blue)
                    'End If
                Next

                SelectedCellDetector()

                If selectedCELLIndex = -1 And SelectedCellCount = 0 And M_Pointer = True Then
                    Form2.UnCheckAllRelationCheckBoxes()
                    Form2.UnCheckAllThematicColorCheckBoxes()
                    Form2.UnCheckAllTACheckBoxes()
                End If
                If selectedCELLIndex <> -1 And SelectedCellCount = 1 And M_Pointer = True And Form2.OutgoingNeighborsCheckBox.Checked = True Then
                    Form2.FillNeighborTable()
                End If
                If selectedCELLIndex <> -1 And SelectedCellCount = 1 And M_Pointer = True And Form2.IncomingNeighborsCheckBox.Checked = True Then
                    Form2.FillIncNeighborTable()
                End If
                If selectedCELLIndex <> -1 And SelectedCellCount = 1 And M_Pointer = True And Form2.PCIConflictCheckBox.Checked = True Then
                    Form2.FillPCIConflictTable()
                End If
                If selectedCELLIndex <> -1 And SelectedCellCount = 1 And M_Pointer = True And Form2.PCIConfusionCheckBox.Checked = True Then
                    Form2.FillPCIConfusionTable()
                End If
                If selectedCELLIndex <> -1 And SelectedCellCount = 1 And M_Pointer = True And Form2.RSIConflictCheckBox.Checked = True Then
                    Form2.FillRSIConflictTable()
                End If
                If selectedCELLIndex <> -1 And SelectedCellCount = 1 And M_Pointer = True And Form2.SingleCheckBox.Checked = True And (Form2.PERCENTILETACheckBox.Checked = True Or Form2.AVGTACheckBox.Checked = True) Then
                    Form2.CleanAVG_PERCENTILETABlockARCs()
                    TA_XPERCENTILE()
                    Form2.Show_Timing_Advance()
                End If
                If SelectedCellCount > 1 And M_Pointer = True And Form2.MultipleCheckBox.Checked = True And (Form2.PERCENTILETACheckBox.Checked = True Or Form2.AVGTACheckBox.Checked = True) Then
                    Form2.CleanAVG_PERCENTILETABlockARCs()
                    TA_XPERCENTILE()
                    Form2.Show_Timing_Advance()
                End If


                is_NE_Dragging = True ' Start dragging when the mouse is pressed
                previousMousePosition = e.Location ' Record the mouse position at the start of dragging


            End If

            Me.Invalidate() ' Redraw the form

        End If

        If (currentMode = DrawingMode.Polygon Or currentMode = DrawingMode.Circle Or currentMode = DrawingMode.Rectangle) And e.Button = MouseButtons.Left Then
            DeSelectAll()
            Me.Invalidate()
        End If


        '###################################################################################################################################################
        If currentMode = DrawingMode.Polygon And (M_Pointer = False And M_Hold = False) Then
            If isPolygonComplete Then
                ' Check if a vertex is clicked for dragging
                If e.Button = MouseButtons.Left Then
                    movingVertexIndex = -1
                    For i As Integer = 0 To vertices.Count - 1
                        If IsPointInPolygonCircle(e.Location, vertices(i), circleRadius) Then
                            movingVertexIndex = i
                            isPolygonDrawing = True ' Start dragging
                            Exit For
                        End If
                    Next
                ElseIf e.Button = MouseButtons.Right Then
                    ' Remove polygon if right-clicking on a vertex
                    For i As Integer = 0 To vertices.Count - 1
                        If IsPointInPolygonCircle(e.Location, vertices(i), circleRadius) Then
                            ResetDrawing() ' Clear the polygon and reset the form

                            Exit For
                        End If
                    Next
                End If
            Else
                If e.Button = MouseButtons.Left Then
                    ' Start drawing the free-hand shape
                    isPolygonDrawing = True
                    freeHandPoints.Clear() ' Clear any previous free-hand points
                    freeHandPoints.Add(e.Location) ' Add the starting point
                End If
            End If
        Else
            ' Other drawing modes (Line, Rectangle, Circle)
            If Not isPolygonDrawing Then
                startPoint = e.Location
                isPolygonDrawing = True
            Else
                startPoint = Nothing
                isPolygonDrawing = False
                Me.Invalidate() ' Clear the form
            End If
        End If
        '###################################################################################################################################################
        'SelectedCellDetector()
        If Form2.RelationLineVisible = True And e.Button = MouseButtons.Left Then
            'Form2.RelationLineCoordinatesUpdate()
        End If

    End Sub

    ' Mouse move event to drag all shapes when the left mouse button is down
    Private Sub Form1_MouseMove(sender As Object, e As MouseEventArgs) Handles MyBase.MouseMove

        Dim TY As Double
        Dim TX As Double
        Dim Calc1 As Double
        Dim Calc2 As Double
        Dim Calc3 As Double

        TY = (BPY * 1000 - e.Y * 1000) / 111000 / OLCEK + BKY
        Calc1 = Math.Sin((TY) * Math.PI / 180) * Math.Sin(TY * Math.PI / 180) + Math.Cos(TY * Math.PI / 180) * Math.Cos((TY) * Math.PI / 180) * Math.Cos(1 * Math.PI / 180 - (1 - 1) * Math.PI / 180)
        Calc2 = Math.Atan(-Calc1 / Math.Sqrt(-Calc1 * Calc1 + 1)) + 2 * Math.Atan(1)
        Calc3 = Math.Round(Calc2 * 6371, 6) * 1000
        'Calc3 = Math.Round(Math.Acos(Calc1) * 6371, 2) * 1000
        TX = ((e.X * 1000 - BPX * 1000) / Calc3 / OLCEK) + BKX
        Coordinates.Text = "Lat:" & TY & " Lon:" & TX

        'GoTo stp1
        '############################################################### Block Arc ###############################################################
        If M_Network_Loaded = True And selectedCELLIndex <> -1 And SelectedCellCount = 1 And Form2.SingleCheckBox.Checked = True And Form2.PERCENTILETACheckBox.Checked = True And isPERCENTILETABlockArcVisible = True And M_Measure = False Then
            Dim Arc_i_x, Arc_i_y, Arc_o_x, Arc_o_y, Arc_c_x, Arc_c_y, outerRadius, innerRadius As Double
            Dim dx_arc As Integer = e.X - CELLS(selectedCELLIndex).pos(0).X
            Dim dy_arc As Integer = e.Y - CELLS(selectedCELLIndex).pos(0).Y
            Dim distance_arc As Double = Math.Sqrt(dx_arc * dx_arc + dy_arc * dy_arc)
            Arc_c_x = CELLS(selectedCELLIndex).pos(0).X
            Arc_c_y = CELLS(selectedCELLIndex).pos(0).Y
            'Distance.Text = distance_arc

            For Arc_Index = 0 To 34 'MaxArcIndex
                If TA_Location(selectedCELLIndex, Arc_Index, 8) > 0 Then
                    If Arc_Index = 0 Then Arc_i_x = CELLS(selectedCELLIndex).pos(0).X
                    If Arc_Index = 0 Then Arc_i_y = CELLS(selectedCELLIndex).pos(0).Y
                    If Arc_Index > 0 Then Arc_i_x = TA_Location(selectedCELLIndex, Arc_Index - 1, 4)
                    If Arc_Index > 0 Then Arc_i_y = TA_Location(selectedCELLIndex, Arc_Index - 1, 3)
                    Arc_o_x = TA_Location(selectedCELLIndex, Arc_Index, 4)
                    Arc_o_y = TA_Location(selectedCELLIndex, Arc_Index, 3)
                    outerRadius = Math.Sqrt((Arc_o_y - Arc_c_y) * (Arc_o_y - Arc_c_y) + (Arc_o_x - Arc_c_x) * (Arc_o_x - Arc_c_x))
                    innerRadius = Math.Sqrt((Arc_i_y - Arc_c_y) * (Arc_i_y - Arc_c_y) + (Arc_i_x - Arc_c_x) * (Arc_i_x - Arc_c_x))

                    If distance_arc >= innerRadius AndAlso distance_arc <= outerRadius Then
                        'Distance.Text = distance_arc
                        'Dim angle As Double = Math.Atan2(dy_arc, dx_arc) * (180 / Math.PI)
                        'If angle < 0 Then angle += 360
                        Dim angle As Double = Math.Atan2(dy_arc, dx_arc) * (180 / Math.PI)
                        angle = angle + 90
                        'If angle < 0 Then angle += 360
                        'Distance.Text = angle

                        If angle >= CELLS(selectedCELLIndex).Azimuth - 33 AndAlso angle <= CELLS(selectedCELLIndex).Azimuth + 33 Then
                            'Distance.Text = Arc_Index & "," & Math.Round(angle, 2) & "," & Math.Round(TA_Location(selectedCELLIndex, Arc_Index, 6), 2)
                            'tooltipTA.Show("Percentile" & Math.Round(TA_Location(selectedCELLIndex, Arc_Index, 6), 2), Me, e.X + 10, e.Y + 10)
                            tooltip.SetToolTip(Me, "TA%:" & Math.Round(TA_Location(selectedCELLIndex, Arc_Index, 8), 3))
                            currentTooltipIndexTA = Arc_Index
                            Exit Sub
                            'tooltipTA.Show(Math.Round(TA_Location(selectedCELLIndex, Arc_Index, 6), 2), Me,e.Location)
                        End If
                    End If

                End If
            Next
            tooltipTA.SetToolTip(Me, "")
            currentTooltipIndexTA = -1
        End If
        '##########################################################################################################################################


        If Form2.RelationLineVisible = True And e.Button = MouseButtons.Left Then
            'Form2.RelationLineCoordinatesUpdate()
        End If
        'stp1:

        If M_Network_Loaded = True And (M_Pointer = True Or M_Hold = True) Then
            If is_NE_Dragging Then
                ' Calculate how much the mouse has moved
                Dim dx As Single = e.X - previousMousePosition.X
                Dim dy As Single = e.Y - previousMousePosition.Y
                BPX = BPX + dx
                BPY = BPY + dy

                Dim deltaX As Integer = e.X - Label_offsetX
                Dim deltaY As Integer = e.Y - Label_offsetY


                ' Move all CELLS by that amount
                For i As Integer = 0 To CELLS.Length - 1 ''''''''''''''''''''3
                    For p As Integer = 0 To 7
                        CELLS(i).pos(p) = New PointF(CELLS(i).pos(p).X + dx, CELLS(i).pos(p).Y + dy)
                    Next
                Next

                ' Move all Polygon by that amount
                For i As Integer = 0 To vertices.Count - 1
                    vertices(i) = New Point(CInt((vertices(i).X) + dx), CInt((vertices(i).Y) + dy))
                Next

                ' Update the previous mouse position to the current position
                previousMousePosition = e.Location
                '   Invalidate the entire form to redraw all CELLS
                If ShowLabels = True Then LabelPosition()
                Me.Invalidate()

                For i As Integer = 0 To CELLS.Length - 1
                    CELL_POSITION(i)
                    GenerateShape(CELLS(i), CELLS(i).CartesianX, CELLS(i).CartesianY, CELLS(i).Azimuth, 33, CLength, CDelta, CELLS(i).Frequency) ' Custom shape generation
                Next
                'GoTo stp3
                If selectedCELLIndex <> -1 And e.Button = MouseButtons.Left And SelectedCellCount = 1 And Form2.SingleCheckBox.Checked = True And (Form2.PERCENTILETACheckBox.Checked = True Or Form2.AVGTACheckBox.Checked = True) Then
                    Form2.CleanAVG_PERCENTILETABlockARCs()
                    Form2.Show_Timing_Advance()
                End If

                If e.Button = MouseButtons.Left And SelectedCellCount > 1 And Form2.MultipleCheckBox.Checked = True And (Form2.PERCENTILETACheckBox.Checked = True Or Form2.AVGTACheckBox.Checked = True) Then
                    Form2.CleanAVG_PERCENTILETABlockARCs()
                    Form2.Show_Timing_Advance()
                End If
                'stp3:
            End If

            ' Check if the mouse is hovering over any arrow
            For i As Integer = 0 To CELLS.Length - 1 ''''''''''''''''''''4
                If IsPointInCell(e.Location, CELLS(i).pos) Then
                    ' Show tooltip if hovering over a different arrow
                    If currentTooltipIndex <> i Then
                        tooltip.SetToolTip(Me, CELLS(i).TooltipText) ' Show tooltip with the text for this arrow
                        currentTooltipIndex = i ' Update the current hovered arrow
                    End If
                    Exit Sub
                End If
            Next

            ' Reset if no arrow is hovered
            tooltip.SetToolTip(Me, "")
            currentTooltipIndex = -1



        End If


        'GoTo stp2


        '#########################################################################################################################
        If currentMode = DrawingMode.Polygon AndAlso isPolygonDrawing And (M_Pointer = False And M_Hold = False) Then
            If isPolygonComplete Then
                ' Move the selected vertex if it is being dragged
                If movingVertexIndex <> -1 Then
                    vertices(movingVertexIndex) = e.Location
                    Me.Invalidate() ' Refresh the form
                End If
            Else
                ' Track free-hand points while drawing
                If freeHandPoints.Count = 0 OrElse DistanceLine(e.Location, freeHandPoints.Last()) > pointDistanceThreshold Then
                    freeHandPoints.Add(e.Location)
                    Me.Invalidate() ' Refresh form to draw the shape
                End If
            End If

        ElseIf isPolygonDrawing AndAlso startPoint.HasValue Then
            ' Other shape drawing (Line, Rectangle, Circle)
            Me.Invalidate()
            Me.Update() ' Force the form to repaint (triggering the Paint event)
        End If
        '#########################################################################################################################
stp2:

    End Sub

    ' Mouse up event to stop dragging
    Private Sub Form1_MouseUp(sender As Object, e As MouseEventArgs) Handles MyBase.MouseUp
        If M_Network_Loaded = True And (M_Pointer = True Or M_Hold = True) Then
            is_NE_Dragging = False ' Stop dragging
        End If

        If Form2.RelationLineVisible = True And e.Button = MouseButtons.Left Then
            'Form2.RelationLineCoordinatesUpdate()
        End If

        '#########################################################################################################################
        If (currentMode = DrawingMode.Line Or currentMode = DrawingMode.Circle Or currentMode = DrawingMode.Rectangle) And isPolygonDrawing = True Then
            isPolygonDrawing = False
            If currentMode = DrawingMode.Rectangle Then Rectangle_Select()
            If currentMode = DrawingMode.Circle Then Circle_Select()
            Me.Invalidate() ' Clear the form
        End If
        '#########################################################################################################################
        '#########################################################################################################################
        If currentMode = DrawingMode.Polygon AndAlso isPolygonDrawing Then
            If isPolygonComplete Then
                ' Stop moving vertices
                isPolygonDrawing = False
                movingVertexIndex = -1
            Else
                ' Finalize the free-hand shape as a polygon when mouse is released
                isPolygonDrawing = False
                vertices = freeHandPoints.ToList() ' Copy the free-hand points as vertices
                isPolygonComplete = True ' Mark the polygon as complete
                Me.Invalidate() ' Refresh the form to draw the final polygon
            End If
        End If
        If currentMode = DrawingMode.Polygon And isPolygonComplete = True Then
            Polygon_Select()
            Me.Invalidate()
        End If

        '#########################################################################################################################
        SelectedCellDetector()
    End Sub
    Private Sub Rectangle_Select()
        Dim minx, miny, maxx, maxy As Double
        'Shape_Start_X
        'Shape_Start_Y
        'Shape_End_X
        'Shape_End_Y
        DeSelectAll()
        minx = Math.Min(Shape_Start_X, Shape_End_X)
        miny = Math.Min(Shape_Start_Y, Shape_End_Y)
        maxx = Math.Max(Shape_Start_X, Shape_End_X)
        maxy = Math.Max(Shape_Start_Y, Shape_End_Y)
        For i As Integer = 0 To CELLS.Length - 1
            If CELLS(i).CartesianX > minx And CELLS(i).CartesianX < maxx And CELLS(i).CartesianY > miny And CELLS(i).CartesianY < maxy Then
                CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                CELLS(i).BorderColor = New Pen(Color.Black, 2)
                CELLS(i).IsSelected = True
            Else
                'CELLS(i).FillColor = New SolidBrush(Color.FromArgb(0, 255, 0))
                'CELLS(i).IsSelected = False
                'CELLS(i).BorderColor = Pens.Black
            End If
        Next
        'SelectedCellDetector()
    End Sub
    Private Sub Circle_Select()
        Dim RDS As Double
        Dim Dist As Double
        DeSelectAll()
        'Math.Sqrt((pt.X - center.X) ^ 2 + (pt.Y - center.Y) ^ 2)
        RDS = Math.Sqrt((Shape_Start_X - Shape_End_X) ^ 2 + (Shape_Start_Y - Shape_End_Y) ^ 2)
        For i As Integer = 0 To CELLS.Length - 1

            Dist = Math.Sqrt((Shape_Start_X - CELLS(i).CartesianX) ^ 2 + (Shape_Start_Y - CELLS(i).CartesianY) ^ 2)
            If Dist < RDS Then
                CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                CELLS(i).BorderColor = New Pen(Color.Black, 2)
                CELLS(i).IsSelected = True
            Else
                'CELLS(i).FillColor = New SolidBrush(Color.FromArgb(0, 255, 0))
                'CELLS(i).IsSelected = False
                'CELLS(i).BorderColor = Pens.Black
            End If
        Next
        'SelectedCellDetector()
    End Sub
    Public Sub DeSelectAll()

        For i As Integer = 0 To CELLS.Length - 1
            CELLS(i).FillColor = New SolidBrush(Color.FromArgb(0, 255, 0))
            CELLS(i).IsSelected = False
            CELLS(i).BorderColor = Pens.Black
            CELLS(i).TooltipText = CELLS(i).CellName
        Next

        SelectedCellDetector()
    End Sub

    Public Sub ToolTipTextDefault()
        For i As Integer = 0 To CELLS.Length - 1
            CELLS(i).TooltipText = CELLS(i).CellName
        Next
    End Sub

    Public Sub DisplayMessage()
        'MessageBox.Show("Hello from Form1!")
    End Sub
    Public Sub SelectAll()
        For i As Integer = 0 To CELLS.Length - 1
            CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
            CELLS(i).IsSelected = True
            CELLS(i).BorderColor = Pens.Black
        Next
        SelectedCellDetector()
    End Sub
    Public Sub SelectedCellDetector()
        SelectedCellCount = 0
        SelectedCellContainer = ""
        For i As Integer = 0 To CELLS.Length - 1
            If CELLS(i).IsSelected = True Then
                SelectedCellCount = SelectedCellCount + 1
                SelectedCellContainer = SelectedCellContainer & CELLS(i).CellName & vbCrLf
            End If
        Next
        Form2.SelectedTextO.Text = SelectedCellContainer

        If SelectedCellCount = 1 Then
            For i As Integer = 0 To CELLS.Length - 1
                If CELLS(i).IsSelected = True Then
                    selectedCELLIndex = i
                End If
            Next
        End If

        ToolStripStatusLabel1.Text = SelectedCellCount & "Cells Selected"
        'If Form2.OutgoingNeighborsCheckBox.Checked = True And SelectedCellCount = 1 And Form2.IsRelationTableFilled = False Then Form2.FillNeighborTable()

    End Sub
    Private Sub Polygon_Select()
        'Dim minx, miny, maxx, maxy As Double
        'Dim Cell_InPoly_Cnt, Polygon_Line_Count As Integer
        'Dim M, N As Double
        'Dim k As Integer
        'DeSelectAll()

        For k As Integer = 0 To CELLS.Length - 1

            Dim j As Integer = vertices.Count - 1
            Dim inside As Boolean = False
            For i As Integer = 0 To vertices.Count - 1
                If ((vertices(i).Y < CELLS(k).CartesianY AndAlso vertices(j).Y >= CELLS(k).CartesianY) OrElse (vertices(j).Y < CELLS(k).CartesianY AndAlso vertices(i).Y >= CELLS(k).CartesianY)) AndAlso
                   (vertices(i).X + (CELLS(k).CartesianY - vertices(i).Y) / (vertices(j).Y - vertices(i).Y) * (vertices(j).X - vertices(i).X) < CELLS(k).CartesianX) Then
                    inside = Not inside
                End If
                j = i
            Next

            If inside = True Then
                CELLS(k).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                CELLS(k).IsSelected = True
                CELLS(k).BorderColor = Pens.Black
            ElseIf inside = False Then
                CELLS(k).FillColor = New SolidBrush(Color.FromArgb(0, 255, 0))
                CELLS(k).IsSelected = False
                CELLS(k).BorderColor = Pens.Black
            End If

        Next
        'SelectedCellDetector()
    End Sub

    ' Function to check if a point is inside a polygon THIS IS FOR CELL TYPE POLYGON, NOT THE RANDOM DRAWN ONE.
    Private Function IsPointInCell(pt As PointF, polygon() As PointF) As Boolean
        Dim j As Integer = polygon.Length - 1
        Dim inside As Boolean = False
        For i As Integer = 0 To polygon.Length - 1
            If ((polygon(i).Y < pt.Y AndAlso polygon(j).Y >= pt.Y) OrElse (polygon(j).Y < pt.Y AndAlso polygon(i).Y >= pt.Y)) AndAlso
               (polygon(i).X + (pt.Y - polygon(i).Y) / (polygon(j).Y - polygon(i).Y) * (polygon(j).X - polygon(i).X) < pt.X) Then
                inside = Not inside
            End If
            j = i
        Next
        Return inside
    End Function

    Private Sub LoadNetwork__Click(sender As Object, e As EventArgs) Handles LoadNetwork_.Click
        LoadNetwork()

    End Sub

    Private Sub Bigger__Click(sender As Object, e As EventArgs) Handles Bigger_.Click
        CDelta = CDbl(CDelta * 1.4)
        CLength = CDbl(CLength * 1.4)
        For i As Integer = 0 To CELLS.Length - 1
            CELL_POSITION(i)
            GenerateShape(CELLS(i), CELLS(i).CartesianX, CELLS(i).CartesianY, CELLS(i).Azimuth, 33, CLength, CDelta, CELLS(i).Frequency) ' Custom shape generation
        Next
        If ShowLabels = True Then LabelPosition()
        Me.Invalidate() ' Redraw the form
    End Sub

    Private Sub Smaller__Click(sender As Object, e As EventArgs) Handles Smaller_.Click
        CDelta = CDbl(CDelta / 1.4)
        CLength = CDbl(CLength / 1.4)
        For i As Integer = 0 To CELLS.Length - 1
            CELL_POSITION(i)
            GenerateShape(CELLS(i), CELLS(i).CartesianX, CELLS(i).CartesianY, CELLS(i).Azimuth, 33, CLength, CDelta, CELLS(i).Frequency) ' Custom shape generation
        Next
        If ShowLabels = True Then LabelPosition()
        Me.Invalidate() ' Redraw the form
    End Sub

    Private Sub Pointer__Click(sender As Object, e As EventArgs) Handles Pointer_.Click
        M_Pointer = True
        M_Hold = False
        M_Polygon = False
        M_Rectangle = False
        M_Circle = False
        M_Measure = False
        currentMode = DrawingMode.None
    End Sub

    Private Sub Hold__Click(sender As Object, e As EventArgs) Handles Hold_.Click
        M_Pointer = False
        M_Hold = True
        M_Polygon = False
        M_Rectangle = False
        M_Circle = False
        M_Measure = False
        currentMode = DrawingMode.None
    End Sub

    Private Sub Rectangle_Select__Click(sender As Object, e As EventArgs) Handles Rectangle_Select_.Click
        M_Pointer = False
        M_Hold = False
        M_Polygon = False
        M_Rectangle = True
        M_Circle = False
        M_Measure = False
        currentMode = DrawingMode.Rectangle
        ResetDrawing()
        Form2.UnCheckAllRelationCheckBoxes()
        Form2.UnCheckAllThematicColorCheckBoxes()
        Form2.UnCheckAllTACheckBoxes()
    End Sub

    Private Sub Circle_Select__Click(sender As Object, e As EventArgs) Handles Circle_Select_.Click
        M_Pointer = False
        M_Hold = False
        M_Polygon = False
        M_Rectangle = False
        M_Circle = True
        M_Measure = False
        currentMode = DrawingMode.Circle
        ResetDrawing()
        Form2.UnCheckAllRelationCheckBoxes()
        Form2.UnCheckAllThematicColorCheckBoxes()
        Form2.UnCheckAllTACheckBoxes()
    End Sub

    Private Sub Polygon_Select__Click(sender As Object, e As EventArgs) Handles Polygon_Select_.Click
        M_Pointer = False
        M_Hold = False
        M_Polygon = True
        M_Rectangle = False
        M_Circle = False
        M_Measure = False
        currentMode = DrawingMode.Polygon
        ResetDrawing()
        Form2.UnCheckAllRelationCheckBoxes()
        Form2.UnCheckAllThematicColorCheckBoxes()
        Form2.UnCheckAllTACheckBoxes()
    End Sub

    Private Sub Meter__Click(sender As Object, e As EventArgs) Handles Meter_.Click
        M_Pointer = False
        M_Hold = False
        M_Polygon = False
        M_Rectangle = False
        M_Circle = False
        M_Measure = True
        'Me.Invalidate() ' Clear the form
        currentMode = DrawingMode.Line
        ResetDrawing()
    End Sub

    Public Sub ResetDrawing()
        isPolygonDrawing = False
        startPoint = Nothing
        vertices.Clear()
        freeHandPoints.Clear()
        movingVertexIndex = -1
        isPolygonComplete = False
        Me.Invalidate() ' Clear the form
        'ResetCount = ResetCount + 1
        'Distance.Text = ResetCount
        SelectedCellDetector()
    End Sub

    Private Sub ZoomInF()
        CLength = CDbl(CLength * 1.4)
        CDelta = CDbl(CDelta * 1.4)
        For i As Integer = 0 To CELLS.Length - 1
            CELL_POSITION(i)
            GenerateShape(CELLS(i), CELLS(i).CartesianX, CELLS(i).CartesianY, CELLS(i).Azimuth, 33, CLength, CDelta, CELLS(i).Frequency) ' Custom shape generation
        Next
        Me.Invalidate() ' Redraw the form
    End Sub
    Private Sub ZoomOutF()
        CLength = CDbl(CLength / 1.4)
        CDelta = CDbl(CDelta / 1.4)
        For i As Integer = 0 To CELLS.Length - 1
            CELL_POSITION(i)
            GenerateShape(CELLS(i), CELLS(i).CartesianX, CELLS(i).CartesianY, CELLS(i).Azimuth, 33, CLength, CDelta, CELLS(i).Frequency) ' Custom shape generation
        Next
        Me.Invalidate() ' Redraw the form
    End Sub

    Private Function DistanceLine(pt1 As Point, pt2 As Point) As Double
        Return Math.Sqrt((pt1.X - pt2.X) ^ 2 + (pt1.Y - pt2.Y) ^ 2)
    End Function

    ' Helper function to check if a point is inside a circle
    Private Function IsPointInPolygonCircle(pt As Point, center As Point, radius As Integer) As Boolean
        Return Math.Sqrt((pt.X - center.X) ^ 2 + (pt.Y - center.Y) ^ 2) <= radius
    End Function
    Private Sub Form1_MouseWheel(sender As Object, e As MouseEventArgs) Handles Me.MouseWheel

        'Form2.RelationLineCoordinatesUpdate()
        'TextBox1.Text = "X: " & e.X & " Y: " & e.Y
        If e.Delta > 0 Then
            'value += 1
            OLCEK = OLCEK * 2
            BPX = 2 * BPX - e.X
            BPY = 2 * BPY - e.Y
            ZoomInF()
            ' Scale polygon vertices for zoom in
            For i As Integer = 0 To vertices.Count - 1
                vertices(i) = New Point(CInt((vertices(i).X - e.X) * 2 + e.X), CInt((vertices(i).Y - e.Y) * 2 + e.Y))
            Next

        Else
            'value -= 1
            OLCEK = OLCEK / 2
            BPX = (e.X + BPX) / 2
            BPY = (e.Y + BPY) / 2
            ZoomOutF()

            ' Scale polygon vertices for zoom out
            For i As Integer = 0 To vertices.Count - 1
                vertices(i) = New Point(CInt((vertices(i).X - e.X) / 2 + e.X), CInt((vertices(i).Y - e.Y) / 2 + e.Y))
            Next

        End If


        If ShowLabels = True Then LabelPosition()

        'Form2.RelationLineCoordinatesUpdate()

        If selectedCELLIndex <> -1 And SelectedCellCount = 1 And Form2.SingleCheckBox.Checked = True And (Form2.PERCENTILETACheckBox.Checked = True Or Form2.AVGTACheckBox.Checked = True) Then
            Form2.CleanAVG_PERCENTILETABlockARCs()
            Form2.Show_Timing_Advance()
        End If
        If SelectedCellCount > 1 And Form2.MultipleCheckBox.Checked = True And (Form2.PERCENTILETACheckBox.Checked = True Or Form2.AVGTACheckBox.Checked = True) Then
            Form2.CleanAVG_PERCENTILETABlockARCs()
            Form2.Show_Timing_Advance()
        End If


        Me.Refresh()



    End Sub

    ' Handle Paint event to draw shapes and polygon vertices
    Private Sub Form1_Paint(sender As Object, e As PaintEventArgs) Handles MyBase.Paint
        Dim g As Graphics = e.Graphics
        Dim Calc1 As Double
        Dim Calc2 As Double
        Dim Calc3 As Double
        Dim i As Integer = -1
        'Dim j As Integer
        Dim pen As New Pen(Color.Black)
        'Public RelationLinestartPoint As Point
        'Public RelationLineendPoint As Point
        '
        If SelectedCellCount > 0 And (Form2.SingleCheckBox.Checked = True Or Form2.MultipleCheckBox.Checked = True) And (Form2.AVGTACheckBox.Checked = True Or Form2.PERCENTILETACheckBox.Checked = True) Then
            '############################################################### Block Arc ###############################################################
            Dim gTA As Graphics = e.Graphics
            gTA.SmoothingMode = SmoothingMode.AntiAlias

            ' Draw all block arcs
            For Each arc In blockArcs
                arc.Draw(gTA)
            Next
            '############################################################### Block Arc ###############################################################
        End If


        If ShowLabels Then
            'For i As Integer = 0 To CELLS.Length - 1
            'LabelLocations.Add(New Point(CELLS(i).pos(4).X, CELLS(i).pos(4).Y))
            'Next
            For Each location As Point In LabelLocations
                i = i + 1
                e.Graphics.DrawString(CELLS(i).Label, Me.Font, Brushes.Black, location)
            Next

        End If

        'GoTo tst1
        'If Form2.RelationLineVisible = True And Form2.RelationCount > 0 And SelectedCellCount = 1 And (Form2.OutgoingNeighborsCheckBox.Checked = True Or Form2.IncomingNeighborsCheckBox.Checked = True Or Form2.PCIConflictCheckBox.Checked = True Or Form2.PCIConfusionCheckBox.Checked = True Or Form2.RSIConflictCheckBox.Checked = True) Then
        'For j = 1 To Form2.RelationCount
        'RelationLinestartPoint = New Point(Form2.RelationCartesianCoordinates(j, 2), Form2.RelationCartesianCoordinates(j, 3))
        'RelationLineendPoint = New Point(Form2.RelationCartesianCoordinates(j, 5), Form2.RelationCartesianCoordinates(j, 6))
        'g.DrawLine(pen, RelationLinestartPoint, RelationLineendPoint)
        'Next
        'End If
        'tst1:
        'If Form2.RelationLineVisible = True And Form2.RelationCount > 0 And SelectedCellCount = 1 And Form2.IncomingNeighborsCheckBox.Checked = True Then
        '   For j = 1 To Form2.RelationCount
        '       RelationLinestartPoint = New Point(Form2.RelationCartesianCoordinates(j, 2), Form2.RelationCartesianCoordinates(j, 3))
        '       RelationLineendPoint = New Point(Form2.RelationCartesianCoordinates(j, 5), Form2.RelationCartesianCoordinates(j, 6))
        '       g.DrawLine(pen, RelationLinestartPoint, RelationLineendPoint)
        '   Next
        'End If

        'If Form2.RelationLineVisible = True And Form2.RelationCount > 0 And SelectedCellCount = 1 And Form2.PCIConflictCheckBox.Checked = True Then
        'For j = 1 To Form2.RelationCount
        'R'elationLinestartPoint = New Point(Form2.RelationCartesianCoordinates(j, 2), Form2.RelationCartesianCoordinates(j, 3))
        'RelationLineendPoint = New Point(Form2.RelationCartesianCoordinates(j, 5), Form2.RelationCartesianCoordinates(j, 6))
        'g.DrawLine(pen, RelationLinestartPoint, RelationLineendPoint)
        'Next
        'End If




        If currentMode = DrawingMode.Polygon Then
            ' Draw the free-hand shape during drawing
            If Not isPolygonComplete AndAlso freeHandPoints.Count > 1 Then
                g.DrawLines(Pens.Black, freeHandPoints.ToArray())
            End If

            ' Draw the polygon if drawing is complete
            If isPolygonComplete AndAlso vertices.Count > 1 Then
                g.DrawPolygon(Pens.BlueViolet, vertices.ToArray())

                ' Draw bigger light blue circles with a black border on each vertex
                For Each vertex As Point In vertices
                    ' Draw filled light blue circle
                    g.FillEllipse(Brushes.LightBlue, vertex.X - circleRadius, vertex.Y - circleRadius, circleRadius * 2, circleRadius * 2)

                    ' Draw black border around the circle
                    g.DrawEllipse(Pens.Black, vertex.X - circleRadius, vertex.Y - circleRadius, circleRadius * 2, circleRadius * 2)
                Next
            End If
        ElseIf isPolygonDrawing AndAlso startPoint.HasValue Then
            ' Handle other shapes (Line, Rectangle, Circle)
            Dim endPoint As Point = Me.PointToClient(MousePosition)

            If currentMode = DrawingMode.Line Or currentMode = DrawingMode.Circle Or currentMode = DrawingMode.Rectangle Then

                Shape_Start_X = startPoint.Value.X
                Shape_Start_Y = startPoint.Value.Y

                Shape_Start_LAT = (BPY * 1000 - Shape_Start_Y * 1000) / 111000 / OLCEK + BKY
                Calc1 = Math.Sin((Shape_Start_LAT) * Math.PI / 180) * Math.Sin(Shape_Start_LAT * Math.PI / 180) + Math.Cos(Shape_Start_LAT * Math.PI / 180) * Math.Cos((Shape_Start_LAT) * Math.PI / 180) * Math.Cos(1 * Math.PI / 180 - (1 - 1) * Math.PI / 180)
                Calc2 = Math.Atan(-Calc1 / Math.Sqrt(-Calc1 * Calc1 + 1)) + 2 * Math.Atan(1)
                Calc3 = Math.Round(Calc2 * 6371, 6) * 1000
                'Calc3 = Math.Round(Math.Acos(Calc1) * 6371, 2) * 1000
                Shape_Start_LON = ((Shape_Start_X * 1000 - BPX * 1000) / Calc3 / OLCEK) + BKX

                Shape_End_X = endPoint.X
                Shape_End_Y = endPoint.Y
                Shape_End_LAT = (BPY * 1000 - Shape_End_Y * 1000) / 111000 / OLCEK + BKY
                Calc1 = Math.Sin((Shape_End_LAT) * Math.PI / 180) * Math.Sin(Shape_End_LAT * Math.PI / 180) + Math.Cos(Shape_End_LAT * Math.PI / 180) * Math.Cos((Shape_End_LAT) * Math.PI / 180) * Math.Cos(1 * Math.PI / 180 - (1 - 1) * Math.PI / 180)
                Calc2 = Math.Atan(-Calc1 / Math.Sqrt(-Calc1 * Calc1 + 1)) + 2 * Math.Atan(1)
                Calc3 = Math.Round(Calc2 * 6371, 6) * 1000
                'Calc3 = Math.Round(Math.Acos(Calc1) * 6371, 2) * 1000
                Shape_End_LON = ((Shape_End_X * 1000 - BPX * 1000) / Calc3 / OLCEK) + BKX

                Dim Measure_angle As Double = Math.Atan2(Shape_End_Y - Shape_Start_Y, Shape_End_X - Shape_Start_X) * (180 / Math.PI)
                Measure_angle = Measure_angle + 90
                If Measure_angle < 0 Then Measure_angle += 360

                Calc1 = Math.Sin((Shape_Start_LAT) * Math.PI / 180) * Math.Sin(Shape_End_LAT * Math.PI / 180) + Math.Cos(Shape_Start_LAT * Math.PI / 180) * Math.Cos((Shape_End_LAT) * Math.PI / 180) * Math.Cos(Shape_End_LON * Math.PI / 180 - Shape_Start_LON * Math.PI / 180)
                Calc2 = Math.Atan(-Calc1 / Math.Sqrt(-Calc1 * Calc1 + 1)) + 2 * Math.Atan(1)
                'Shape_Length = Math.Round(Math.Acos(Calc1) * 6371, 2) * 1000
                Shape_Length = Math.Round(Calc2 * 6371, 6) '* 1000
                Distance.Text = Shape_Length

                Dim lengthText As String = Shape_Length.ToString("0.00") & " km" & " " & Math.Round(Measure_angle, 2) & "°"
                Dim font As New Font("Arial", 10)
                Dim textPosition As New Point(endPoint.X + 5, endPoint.Y - 15) ' Slight offset from the end point
                e.Graphics.DrawString(lengthText, font, Brushes.Black, textPosition)
            End If
            Select Case currentMode
                Case DrawingMode.Line
                    ' Draw Line
                    g.DrawLine(Pens.Black, startPoint.Value, endPoint)

                Case DrawingMode.Rectangle
                    ' Draw Rectangle
                    Dim rect As New Rectangle(Math.Min(startPoint.Value.X, endPoint.X),
                                          Math.Min(startPoint.Value.Y, endPoint.Y),
                                          Math.Abs(startPoint.Value.X - endPoint.X),
                                          Math.Abs(startPoint.Value.Y - endPoint.Y))
                    g.DrawRectangle(Pens.Black, rect)

                Case DrawingMode.Circle
                    ' Draw Circle
                    Dim radius As Double = Math.Sqrt(Math.Pow(endPoint.X - startPoint.Value.X, 2) + Math.Pow(endPoint.Y - startPoint.Value.Y, 2))
                    Dim rect As New Rectangle(CInt(startPoint.Value.X - radius), CInt(startPoint.Value.Y - radius), CInt(radius * 2), CInt(radius * 2))
                    g.DrawEllipse(Pens.Black, rect)
            End Select
        End If
    End Sub

    Private Sub Fit__Click(sender As Object, e As EventArgs) Handles Fit_.Click

        ResetDrawing()


        Dim minx, miny, maxx, maxy, zoomrate, deltax, deltay, shiftx, shifty As Double

        'CELLS(i).pos(0).X

        If SelectedCellCount > 0 Then
            For k = 1 To 2


                For i As Integer = 0 To CELLS.Length - 1
                    If CELLS(i).IsSelected = True Then
                        minx = CELLS(i).pos(0).X
                        miny = CELLS(i).pos(0).Y
                        maxx = CELLS(i).pos(0).X
                        maxy = CELLS(i).pos(0).Y
                        GoTo Str1
                    End If
                Next
Str1:


                For i As Integer = 0 To CELLS.Length - 1
                    If CELLS(i).IsSelected = True Then
                        If CELLS(i).pos(0).X < minx Then minx = CELLS(i).pos(0).X
                        If CELLS(i).pos(0).Y < miny Then miny = CELLS(i).pos(0).Y
                        If CELLS(i).pos(0).X > maxx Then maxx = CELLS(i).pos(0).X
                        If CELLS(i).pos(0).Y > maxy Then maxy = CELLS(i).pos(0).Y
                    End If
                Next

                'If Not ((maxx - minx) > Me.Width) Or ((maxy - miny) > Me.Height) Then
                deltax = Me.Width / 2 - (minx + maxx) / 2
                deltay = Me.Height / 2 - (miny + maxy) / 2
                BPX = BPX + deltax
                BPY = BPY + deltay

                'End If

                For i As Integer = 0 To CELLS.Length - 1
                    CELL_POSITION(i)
                    GenerateShape(CELLS(i), CELLS(i).CartesianX, CELLS(i).CartesianY, CELLS(i).Azimuth, 33, CLength, CDelta, CELLS(i).Frequency) ' Custom shape generation
                Next
                Me.Invalidate() ' Redraw the form



                For i As Integer = 0 To CELLS.Length - 1
                    If CELLS(i).IsSelected = True Then
                        minx = CELLS(i).pos(0).X
                        miny = CELLS(i).pos(0).Y
                        maxx = CELLS(i).pos(0).X
                        maxy = CELLS(i).pos(0).Y
                        GoTo Str2
                    End If
                Next
Str2:

                For i As Integer = 0 To CELLS.Length - 1
                    If CELLS(i).IsSelected = True Then
                        If CELLS(i).pos(0).X < minx Then minx = CELLS(i).pos(0).X
                        If CELLS(i).pos(0).Y < miny Then miny = CELLS(i).pos(0).Y
                        If CELLS(i).pos(0).X > maxx Then maxx = CELLS(i).pos(0).X
                        If CELLS(i).pos(0).Y > maxy Then maxy = CELLS(i).pos(0).Y
                    End If
                Next

                If minx = maxx And miny = maxy Then
                    zoomrate = 2
                    shiftx = (minx + maxx) / 2
                    shifty = (miny + maxy) / 2
                Else
                    zoomrate = 1 / Math.Max(((maxx - minx) / Me.Width), ((maxy - miny) / Me.Height))
                    shiftx = 0
                    shifty = 0
                End If

                'If Not ((maxx - minx) > Me.Width) Or ((maxy - miny) > Me.Height) Then
                'zoomrate = 2
                'MsgBox zoomrate
                zoomrate = zoomrate * 0.9
                OLCEK = OLCEK * zoomrate
                BPX = zoomrate * BPX - shiftx
                BPY = zoomrate * BPY - shifty
                CLength = CDbl(CLength * 1.4)
                CDelta = CDbl(CDelta * 1.4)
                For i As Integer = 0 To CELLS.Length - 1
                    CELL_POSITION(i)
                    GenerateShape(CELLS(i), CELLS(i).CartesianX, CELLS(i).CartesianY, CELLS(i).Azimuth, 33, CLength, CDelta, CELLS(i).Frequency) ' Custom shape generation
                Next
                Me.Invalidate() ' Redraw the form


                'Distance.Text = Me.Width & "," & Me.Height & "," & (minx + maxx) / 2 & "," & (miny + maxy) / 2


            Next
        End If
    End Sub

    Private Sub Find__Click(sender As Object, e As EventArgs) Handles Find_.Click
        Dim deltax, deltay As Double
        DeSelectAll()
        For i As Integer = 0 To CELLS.Length - 1
            If CELLS(i).CellName = Search_.Text And Len(Search_.Text) > 3 Then
                deltax = Me.Width / 2 - CELLS(i).pos(0).X
                deltay = Me.Height / 2 - CELLS(i).pos(0).Y
                BPX = BPX + deltax
                BPY = BPY + deltay
                CELLS(i).IsSelected = True
                CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0)) ' Custom RGB color
                CELLS(i).BorderColor = New Pen(Color.Black, 2)
                SelectedCellDetector()
                CLength = CDbl(CLength * 1.4)
                CDelta = CDbl(CDelta * 1.4)
                OLCEK = OLCEK * 2
                BPX = BPX * 2 - Me.Width / 2 'CELLS(i).pos(0).X
                BPY = BPY * 2 - Me.Height / 2 'CELLS(i).pos(0).Y
                For j As Integer = 0 To CELLS.Length - 1
                    CELL_POSITION(j)
                    GenerateShape(CELLS(j), CELLS(j).CartesianX, CELLS(j).CartesianY, CELLS(j).Azimuth, 33, CLength, CDelta, CELLS(j).Frequency) ' Custom shape generation
                Next
                Me.Invalidate() ' Redraw the form

            End If
        Next

    End Sub


    Private Sub Form1_Resize(sender As Object, e As EventArgs) Handles Me.Resize
        If M_Network_Loaded = True Then
            For i As Integer = 0 To CELLS.Length - 1
                CELL_POSITION(i)
                GenerateShape(CELLS(i), CELLS(i).CartesianX, CELLS(i).CartesianY, CELLS(i).Azimuth, 33, CLength, CDelta, CELLS(i).Frequency) ' Custom shape generation
            Next
            Me.Invalidate() ' Redraw the form
        End If
    End Sub


End Class
Public Class BlockArc
    Public Property Center As Point
    Public Property StartAngle As Single
    Public Property SweepAngle As Single
    Public Property Color As Color
    Public Property OuterRadius As Double
    Public Property InnerRadius As Double

    Public Sub New(center As Point, startAngle As Single, sweepAngle As Single, color As Color, outerRadius As Integer, innerRadius As Integer)
        Me.Center = center
        Me.StartAngle = startAngle
        Me.SweepAngle = sweepAngle
        Me.Color = color
        Me.OuterRadius = outerRadius
        Me.InnerRadius = innerRadius
    End Sub

    Public Sub Draw(g As Graphics)
        ' Define the bounding rectangles for the outer and inner arcs

        If InnerRadius < 1 Then InnerRadius = 1
        If OuterRadius < 2 Then OuterRadius = 2
        Dim outerRect As New Rectangle(Center.X - OuterRadius, Center.Y - OuterRadius, 2 * OuterRadius, 2 * OuterRadius)
        Dim innerRect As New Rectangle(Center.X - InnerRadius, Center.Y - InnerRadius, 2 * InnerRadius, 2 * InnerRadius)



        'Form2.TextBox1.Text = Center.X - OuterRadius & "," & Center.Y - OuterRadius & "," & 2 * OuterRadius & "," & 2 * OuterRadius & vbCrLf
        'Form2.TextBox1.Text = Form2.TextBox1.Text & "," & Center.X - InnerRadius & "," & Center.Y - InnerRadius & "," & 2 * InnerRadius & "," & 2 * InnerRadius
        Using path As New GraphicsPath()
            ' Add the outer arc
            path.AddArc(outerRect, StartAngle, SweepAngle)

            ' Add the inner arc (in reverse direction)
            path.AddArc(innerRect, StartAngle + SweepAngle, -SweepAngle)

            ' Close the path to form a ring segment
            path.CloseFigure()

            ' Fill the block arc with the specified color
            Using brush As New SolidBrush(Color)
                g.FillPath(brush, path)
            End Using

            Using pen As New Pen(Color.FromArgb(255, 0, 0, 0), 1) ' Fully opaque border
                g.DrawPath(pen, path)
            End Using

        End Using
    End Sub

    ' Check if a point is inside the block arc
    Public Function ContainsPoint(p As Point) As Boolean
        ' Calculate distance from the center to the point
        Dim dx As Integer = p.X - Center.X
        Dim dy As Integer = p.Y - Center.Y
        Dim distance As Double = Math.Sqrt(dx * dx + dy * dy)

        ' Check if the point is within the radii
        If distance >= InnerRadius AndAlso distance <= OuterRadius Then
            ' Calculate angle of the point
            Dim angle As Double = Math.Atan2(dy, dx) * (180 / Math.PI)
            If angle < 0 Then angle += 360

            ' Check if the point is within the arc's angle range
            Return angle >= StartAngle AndAlso angle <= StartAngle + SweepAngle
        End If

        Return False
    End Function
End Class