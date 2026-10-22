Imports System.Data.Common
Imports System.Reflection
Imports System.Runtime.InteropServices.JavaScript.JSType
Imports System.Windows.Forms.VisualStyles.VisualStyleElement
Imports System.Windows.Forms.VisualStyles.VisualStyleElement.Rebar
Imports System.Drawing
Imports MAP6.Form1
Imports System
Imports System.IO
Imports Microsoft.VisualBasic.ApplicationServices
Imports System.Windows.Forms.DataVisualization.Charting

Public Class Form2
    Private form1 As Form1 ' Class-level variable to hold the instance of Form1
    Public RelationCartesianCoordinates(5000, 8) As Double
    Public RelationCount As Integer = 0
    Public RelationLineVisible As Boolean = False
    Public IsRelationTableFilled As Boolean = False
    Public ComplementaryColors(,) As Integer
    Private pdfCounter As Integer() = New Integer(34) {}
    Private TApdfCounter As Double() = New Double(34) {}
    Private Sub Form2_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Initialize form1 using the class-level variable
        form1 = New Form1()
        form1.MdiParent = Me
        form1.Show()

        SizeChanger()

        'ComboBox1.Items.Clear()
        ComboBox2O.Items.Clear()
        ComboBox2O.Items.Add("=")
        ComboBox2O.Items.Add(">")
        ComboBox2O.Items.Add(">=")
        ComboBox2O.Items.Add("<")
        ComboBox2O.Items.Add("<=")
        ComboBox2O.Items.Add("between")
        ComboBox2O.Items.Add("in")
        ComboBox2O.Items.Add("contains")
        ComboBox2O.Items.Add("begins")

        ' Configure the chart
        ConfigureTAChart()

        ' Populate initial data
        UpdateTAChart(TApdfCounter)

    End Sub

    Private Sub Splitter1_SplitterMoved(sender As Object, e As SplitterEventArgs) Handles Splitter1.SplitterMoved
        SizeChanger()
    End Sub
    Private Sub SizeChanger()
        GroupBox2.Width = SplitContainer1.Width - 15
        GroupBox2.Height = SplitContainer1.Panel2.Height - 10
        TabControl1.Width = SplitContainer1.Width - 45
        TabControl1.Height = SplitContainer1.Panel2.Height - 30
        TabControl2.Top = 10
        TabControl2.Width = TabControl1.Width - 20
        TabControl2.Height = SplitContainer1.Panel2.Height - 70
        TabControl30.Top = 10
        TabControl30.Width = TabControl1.Width - 20
        TabControl30.Height = SplitContainer1.Panel2.Height - 70

        SelectedTextO.Width = TabControl30.Width - 15
        SelectedTextO.Height = TabControl30.Height - 70

        AddParameterstoSelectedButtonO.Left = SelectedTextO.Left
        AddParameterstoSelectedButtonO.Top = TabControl30.Top + SelectedTextO.Height + 2

        GroupBox1.Width = SplitContainer1.Width - 15
        GroupBox1.Height = SplitContainer1.Panel1.Height - 10

        GroupBox3.Width = TabControl2.Width - 23
        ComboBox3.Width = TabControl2.Width - 23
        ListView1.Width = TabControl2.Width - 23
        ListView1.Height = TabControl2.Height - 205
        ListView2.Width = TabControl2.Width
        ListView2.Height = TabControl2.Height - 100
        ListView1.Left = GroupBox3.Left
        ApplyButton.Left = GroupBox3.Left
        ComboBox3.Left = GroupBox3.Left
        RandomCheckBox.Left = ComplementaryCheckBox.Left

        SelectFromListO.Left = SelectedTextO.Left
        SelectFromListO.Top = SelectedTextO.Top
        SelectFromListO.Width = SelectedTextO.Width
        SelectFromListO.Height = SelectedTextO.Height - 50
        SelectFromList_o.Left = SelectFromListO.Left
        SelectFromList_o.Top = SelectFromListO.Top + SelectFromListO.Height + 20
        Select_All_O.Top = SelectFromList_o.Top
        Select_All_O.Left = SelectFromListO.Left + SelectFromListO.Width - Select_All_O.Width

        LabelTextBox.Left = ComboBox3.Left
        'LabelTextBox.Top = ComboBox3.Top
        LabelTextBox.Width = SelectedTextO.Width
        LabelListBox.Left = ComboBox3.Left
        LabelListBox.Width = SelectedTextO.Width
        LabelListBox.Height = SelectedTextO.Height - 70
        AddLabelButton.Left = LabelListBox.Left
        AddLabelButton.Top = LabelListBox.Top + LabelListBox.Height + 10
        RemoveLabelButton.Top = AddLabelButton.Top
        RemoveLabelButton.Left = AddLabelButton.Left + SelectFromListO.Width - RemoveLabelButton.Width
        TooltipCheckBox.Top = RemoveLabelButton.Top + 35
        TooltipCheckBox.Left = AddLabelButton.Left + SelectFromListO.Width - TooltipCheckBox.Width

        AddSiteLabelButton.Top = AddLabelButton.Top + AddLabelButton.Height + 10

        'Button1.Left = TabControl2.Left
        TabControl4.Left = TabControl2.Left
        TabControl4.Height = TabControl2.Height
        TabControl4.Width = TabControl2.Width
        TA_CHART.Left = Button2.Left
        TA_CHART.Width = TabControl4.Width - 20
        TA_CHART.Height = 250 'ListView1.Height
        'TA_CHART.Height = (TabControl4.Height - 140) / 2

        TabControl5.Width = TabControl2.Width - 20
        TabControl5.Height = TabControl2.Height - 40


        'GroupBox4.Left = TA_CHART.Left
        'GroupBox4.Width = TA_CHART.Width   ''TabControl4  
        'GroupBox4.Height = TabControl4.Height - TA_CHART.Height - 120
        'GroupBox4.Top = TA_CHART.Top + TA_CHART.Height + 20

        TA_ListView.Left = TA_CHART.Left
        TA_ListView.Top = TA_CHART.Top + TA_CHART.Height + 10
        TA_ListView.Height = TabControl4.Height - TA_CHART.Height - 125
        TA_ListView.Width = TA_CHART.Width


        IncOvershooterListView.Top = TA_ListView.Top
        IncOvershooterListView.Left = TA_ListView.Left
        IncOvershooterListView.Height = TA_ListView.Height
        IncOvershooterListView.Width = TA_ListView.Width

    End Sub
    Private Sub SplitContainer1_SplitterMoved(sender As Object, e As SplitterEventArgs) Handles SplitContainer1.SplitterMoved
        SizeChanger()
    End Sub

    Public Sub SelectFromList__Click(sender As Object, e As EventArgs) Handles SelectFromList_o.Click

        UnCheckAllRelationCheckBoxes()

        ' Check if form1 is not Nothing before calling the DeSelectAll method
        If form1 IsNot Nothing Then
            form1.DeSelectAll()

            For i = 0 To form1.CELLS.Length - 1
                If InStr(SelectFromListO.Text, form1.CELLS(i).CellName) And SelectFromListO.Text <> "" Then
                    form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                    form1.CELLS(i).IsSelected = True
                    form1.CELLS(i).BorderColor = New Pen(Color.Black, 2)
                End If
            Next
            form1.SelectedCellDetector()

            form1.Refresh()
        Else
            'MessageBox.Show("Form1 is not open.")
        End If
    End Sub

    Private Sub Select_All__Click(sender As Object, e As EventArgs) Handles Select_All_O.Click
        UnCheckAllRelationCheckBoxes()

        ' Implement functionality for Select_All_ if needed
        If form1 IsNot Nothing Then
            form1.DeSelectAll()

            For i = 0 To form1.CELLS.Length - 1
                form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                form1.CELLS(i).IsSelected = True
                form1.CELLS(i).BorderColor = New Pen(Color.Black, 2)
            Next
            form1.SelectedCellDetector()

            form1.Refresh()
        Else
            'MessageBox.Show("Form1 is not open.")
        End If
    End Sub

    Private Sub ComboBox1_TextChanged(sender As Object, e As EventArgs)
        'ComboBox1.SelectionStart = ComboBox1.Text.Length
        'If Not ComboBox1.Text = "" Then
        'ComboBox1.Items.Clear()
        'ComboBox1.Refresh()
        'For i = 0 To form1.ParameterCount
        'If InStr(form1.CELLDATA(0, i), ComboBox1.Text) Then ComboBox1.Items.Add(Trim(form1.CELLDATA(0, i)))

        'Next
        'ComboBox1.Refresh()
        'ComboBox1.SelectionStart = ComboBox1.Text.Length
        'End If
        'ComboBox1.Refresh()
        'ComboBox1.SelectionStart = ComboBox1.Text.Length
    End Sub
    Private Sub ApplyButton_Click(sender As Object, e As EventArgs) Handles ApplyButton.Click

        UnCheckAllRelationCheckBoxes()
        ComplementaryColors_()
        Dim ParameterName As String
        Dim ParameterValues As String = ""
        Dim CurrentParameterValue As String = ""
        Dim Column As Integer = -1
        Dim Count, r, r1, g, g1, b, b1 As Integer
        Dim rand As New Random()


        If ComboBox3.Text = "" Then Exit Sub
        If ComplementaryCheckBox.Checked = 0 And HueCheckBox.Checked = 0 And RandomCheckBox.Checked = 0 Then Exit Sub


        form1.DeSelectAll()

        'form1.SelectedGradientColorIndex = 0
        ListView1.Clear()
        ListView1.View = View.Details


        ParameterName = ComboBox3.Text

        If ComboBox3.Text = "" Then Exit Sub
        For i = 0 To form1.ParameterCount
            If ParameterName = form1.CELLDATA(0, i) Then Column = i
        Next


        If Column > -1 Then
            ParameterValues = "#" & form1.CELLDATA(1, Column) & "#|"
            For i = 2 To form1.CELLS.Length
                If InStr(ParameterValues, "#" & form1.CELLDATA(i, Column) & "#|") = 0 Then
                    ParameterValues = ParameterValues & "#" & form1.CELLDATA(i, Column) & "#|"
                End If
            Next
        End If
        ParameterValues = Replace(ParameterValues, "#", "")

        ParameterValues = ParameterValues.Substring(0, ParameterValues.Length - 1)

        Dim result As String = SortStringDescending(ParameterValues)
        'MsgBox(ParameterValues)
        'MsgBox(result)
        Dim DiffParameterValues() As String = Split(result, "|")


        'MsgBox(UBound(DiffParameterValues))
        ListView1.Columns.Add(UBound(DiffParameterValues) + 1 & " Variants", 100, HorizontalAlignment.Left)
        ListView1.Columns.Add("Color", 150, HorizontalAlignment.Left)

        For i = 0 To UBound(DiffParameterValues)
            CurrentParameterValue = Replace(DiffParameterValues(i), "#", "")
            Count = 0
            For j = 1 To form1.CELLS.Length
                If CurrentParameterValue = form1.CELLDATA(j, Column) Then
                    Count = Count + 1
                End If
            Next
            Dim listItem As New ListViewItem(CurrentParameterValue & "(#" & Count & ")") ' First column

            ' Add the second column
            Dim subItem As New ListViewItem.ListViewSubItem()
            subItem.Text = ""

            If RandomCheckBox.Checked Then
                subItem.BackColor = Color.FromArgb(rand.Next(0, 256), rand.Next(0, 256), rand.Next(0, 256))
                For j = 1 To form1.CELLS.Length
                    If CurrentParameterValue = form1.CELLDATA(j, Column) Then
                        form1.CELLS(j - 1).FillColor = New SolidBrush(subItem.BackColor)
                        form1.CELLS(j - 1).BorderColor = Pens.Black '
                        form1.CELLS(j - 1).TooltipText = form1.CELLS(j - 1).CellName & ":" & form1.CELLDATA(j, Column)
                    End If
                Next
            End If

            If ComplementaryCheckBox.Checked Then
                If UBound(DiffParameterValues) < 78 Then
                    subItem.BackColor = Color.FromArgb(ComplementaryColors(i, 0), ComplementaryColors(i, 1), ComplementaryColors(i, 2))
                    For j = 1 To form1.CELLS.Length
                        If CurrentParameterValue = form1.CELLDATA(j, Column) Then
                            form1.CELLS(j - 1).FillColor = New SolidBrush(subItem.BackColor)
                            form1.CELLS(j - 1).BorderColor = Pens.Black '
                            form1.CELLS(j - 1).TooltipText = form1.CELLS(j - 1).CellName & ":" & form1.CELLDATA(j, Column)
                        End If
                    Next
                Else ComplementaryCheckBox.Checked = False
                End If
            End If


            If HueCheckBox.Checked Then
                r = 255
                g = 255
                b = 255
                If form1.SelectedGradientColorIndex = 1 Then
                    r1 = 255
                    g1 = (i + 1) * g / (UBound(DiffParameterValues) + 1)
                    b1 = (i + 1) * b / (UBound(DiffParameterValues) + 1)
                End If
                If form1.SelectedGradientColorIndex = 2 Then
                    r1 = (i + 1) * r / (UBound(DiffParameterValues) + 1)
                    g1 = 255
                    b1 = (i + 1) * b / (UBound(DiffParameterValues) + 1)
                End If
                If form1.SelectedGradientColorIndex = 3 Then
                    r1 = (i + 1) * r / (UBound(DiffParameterValues) + 1)
                    g1 = (i + 1) * g / (UBound(DiffParameterValues) + 1)
                    b1 = 255
                End If
                If form1.SelectedGradientColorIndex = 4 Then
                    r1 = (i + 1) * r / (UBound(DiffParameterValues) + 1)
                    g1 = 255
                    b1 = 255
                End If
                If form1.SelectedGradientColorIndex = 5 Then
                    r1 = 255
                    g1 = (i + 1) * g / (UBound(DiffParameterValues) + 1)
                    b1 = 255
                End If
                If form1.SelectedGradientColorIndex = 6 Then
                    r1 = 255
                    g1 = 255
                    b1 = (i + 1) * b / (UBound(DiffParameterValues) + 1)
                End If
                subItem.BackColor = Color.FromArgb(r1, g1, b1)
                For j = 1 To form1.CELLS.Length
                    If CurrentParameterValue = form1.CELLDATA(j, Column) Then
                        form1.CELLS(j - 1).FillColor = New SolidBrush(subItem.BackColor)
                        form1.CELLS(j - 1).BorderColor = Pens.Black '
                        form1.CELLS(j - 1).TooltipText = form1.CELLS(j - 1).CellName & ":" & form1.CELLDATA(j, Column)
                    End If
                Next
            End If



            listItem.UseItemStyleForSubItems = False
            listItem.SubItems.Add(subItem)
            ' Add the item to the ListView
            ListView1.Items.Add(listItem)

        Next
        form1.Refresh()
    End Sub
    Public Function SortStringDescending(input As String) As String
        ' Split the input string by "|" and trim any extra spaces
        Dim values() As String = input.Split("|"c).Select(Function(s) s.Trim()).ToArray()

        ' Check if all values are numerical
        Dim numericValues As New List(Of Double)
        For Each value As String In values
            Dim number As Double
            If Double.TryParse(value, number) Then
                numericValues.Add(CDbl(number))
            Else
                ' Return the original input if any value is not numerical
                Return input

            End If
        Next
        'MsgBox(numericValues)
        ' Sort the numeric values in descending order
        'numericValues = numericValues.OrderByDescending(Of Number)
        'numericValues.Sort(numericValues)

        'numericValues.Reverse()

        'Array.Sort(values)
        numericValues.Sort()
        numericValues.Reverse()


        ' Join the sorted values with "|"
        Return String.Join("|", numericValues)
        'Return String.Join("|", values)
    End Function
    Private Sub Select_Button_Click(sender As Object, e As EventArgs) Handles Select_ButtonO.Click

        UnCheckAllRelationCheckBoxes()

        Dim ParameterName As String
        Dim Column = -1
        Dim lenn As Integer
        ParameterName = ComboBox1O.Text
        Dim inputText As String
        inputText = ""

        Dim minn, maxx As Double


        For i = 0 To form1.ParameterCount
            If ParameterName = form1.CELLDATA(0, i) Then Column = i
        Next

        form1.DeSelectAll()

        If ComboBox2O.Text = "=" And Column <> -1 Then
            For i = 1 To form1.CELLS.Length
                If form1.CELLDATA(i, Column) = SelectWithRuleTextO.Text Then
                    form1.CELLS(i - 1).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                    form1.CELLS(i - 1).IsSelected = True
                    form1.CELLS(i - 1).BorderColor = New Pen(Color.Black, 2)
                End If
            Next
        End If



        If ComboBox2O.Text = ">" And Column <> -1 And IsNumeric(SelectWithRuleTextO.Text) Then
            For i = 1 To form1.CELLS.Length
                If form1.CELLDATA(i, Column) > Convert.ToDouble(SelectWithRuleTextO.Text) Then
                    form1.CELLS(i - 1).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                    form1.CELLS(i - 1).IsSelected = True
                    form1.CELLS(i - 1).BorderColor = New Pen(Color.Black, 2)
                End If
            Next
        End If




        If ComboBox2O.Text = ">=" And Column <> -1 And IsNumeric(SelectWithRuleTextO.Text) Then
            For i = 1 To form1.CELLS.Length
                If form1.CELLDATA(i, Column) >= Convert.ToDouble(SelectWithRuleTextO.Text) Then
                    form1.CELLS(i - 1).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                    form1.CELLS(i - 1).IsSelected = True
                    form1.CELLS(i - 1).BorderColor = New Pen(Color.Black, 2)
                End If
            Next
        End If




        If ComboBox2O.Text = "<" And Column <> -1 And IsNumeric(SelectWithRuleTextO.Text) Then
            For i = 1 To form1.CELLS.Length
                If form1.CELLDATA(i, Column) < Convert.ToDouble(SelectWithRuleTextO.Text) Then
                    form1.CELLS(i - 1).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                    form1.CELLS(i - 1).IsSelected = True
                    form1.CELLS(i - 1).BorderColor = New Pen(Color.Black, 2)
                End If
            Next
        End If




        If ComboBox2O.Text = "<=" And Column <> -1 And IsNumeric(SelectWithRuleTextO.Text) Then
            For i = 1 To form1.CELLS.Length
                If form1.CELLDATA(i, Column) <= Convert.ToDouble(SelectWithRuleTextO.Text) Then
                    form1.CELLS(i - 1).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                    form1.CELLS(i - 1).IsSelected = True
                    form1.CELLS(i - 1).BorderColor = New Pen(Color.Black, 2)
                End If
            Next
        End If




        If ComboBox2O.Text = "between" And Column <> -1 Then
            Dim values = SelectWithRuleTextO.Text.Split(","c)
            maxx = Math.Max(Convert.ToDouble(values(0)), Convert.ToDouble(values(1)))
            minn = Math.Min(Convert.ToDouble(values(0)), Convert.ToDouble(values(1)))

            For i = 1 To form1.CELLS.Length
                If form1.CELLDATA(i, Column) >= minn And form1.CELLDATA(i, Column) <= maxx Then
                    form1.CELLS(i - 1).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                    form1.CELLS(i - 1).IsSelected = True
                    form1.CELLS(i - 1).BorderColor = New Pen(Color.Black, 2)
                End If
            Next


        End If


        If ComboBox2O.Text = "in" And Column <> -1 Then
            Dim values = SelectWithRuleTextO.Text.Split(","c)
            For j = 0 To values.Length - 1
                For i = 1 To form1.CELLS.Length
                    If form1.CELLDATA(i, Column) = values(j) Then
                        form1.CELLS(i - 1).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                        form1.CELLS(i - 1).IsSelected = True
                        form1.CELLS(i - 1).BorderColor = New Pen(Color.Black, 2)
                    End If
                Next
            Next
        End If





        If ComboBox2O.Text = "begins" And Column <> -1 Then
            lenn = Len(SelectWithRuleTextO.Text)
            For i = 1 To form1.CELLS.Length
                inputText = form1.CELLDATA(i, Column)
                If inputText.Substring(0, lenn) = SelectWithRuleTextO.Text Then
                    form1.CELLS(i - 1).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                    form1.CELLS(i - 1).IsSelected = True
                    form1.CELLS(i - 1).BorderColor = New Pen(Color.Black, 2)
                End If
            Next
        End If





        If ComboBox2O.Text = "contains" And Column <> -1 Then
            For i = 1 To form1.CELLS.Length
                If InStr(form1.CELLDATA(i, Column), SelectWithRuleTextO.Text) Then
                    form1.CELLS(i - 1).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                    form1.CELLS(i - 1).IsSelected = True
                    form1.CELLS(i - 1).BorderColor = New Pen(Color.Black, 2)
                End If
            Next
        End If



        form1.SelectedCellDetector()
        form1.Refresh()

    End Sub

    Private Sub LabelTextBox_TextChanged(sender As Object, e As EventArgs) Handles LabelTextBox.TextChanged
        Dim filterText As String = LabelTextBox.Text
        form1.Parameters = form1.ParameterString.Split("#"c).ToList()
        LabelListBox.Items.Clear()
        For Each param As String In form1.Parameters
            If param.Contains(filterText, StringComparison.OrdinalIgnoreCase) Then
                LabelListBox.Items.Add(param)
            End If
        Next
    End Sub

    Private Sub AddLabelButton_Click(sender As Object, e As EventArgs) Handles AddLabelButton.Click
        Dim selectedItems As New List(Of String)
        Dim Column As Integer
        Dim Findex As Integer
        Dim NX, NY, LBLX, LBLY As Long

        form1.LabelLocations.Clear()
        form1.ShowLabels = False
        form1.Refresh()
        form1.Invalidate()

        form1.ToolTipTextDefault()


        form1.ShowLabels = True

        For i = 1 To form1.CELLS.Length
            form1.CELLS(i - 1).Label = ""
        Next


        ' Loop through each selected item in the ListBox
        For Each item As String In LabelListBox.SelectedItems
            'selectedItems.Add(item) ' Add the selected item to the list
            Column = -1
            For i = 0 To form1.ParameterCount
                If item = form1.CELLDATA(0, i) Then Column = i
            Next
            'MsgBox(LabelListBox.SelectedItems.Count)
            If Column > -1 Then
                For i = 1 To form1.CELLS.Length
                    form1.CELLS(i - 1).Label = form1.CELLS(i - 1).Label & form1.CELLDATA(i, Column) & "|"
                    'form1.LabelLocations.Add(New Point(form1.CELLS(i - 1).pos(4).X, form1.CELLS(i - 1).pos(4).Y))
                    If TooltipCheckBox.Checked = True Then form1.CELLS(i - 1).TooltipText = form1.CELLS(i - 1).CellName & ":" & form1.CELLS(i - 1).Label
                Next

            End If
        Next

        'MsgBox(form1.CELLDATA(0, Column))
        'MsgBox(form1.CELLDATA(1, Column))
        'MsgBox(form1.CELLDATA(2, Column))


        Dim Size2 As Single = (form1.CLength + 1.5F * (form1.CDelta * (10 + 2)))

        For i = 1 To form1.CELLS.Length
            If form1.CELLS(i - 1).Frequency = 550 Then Findex = 1
            If form1.CELLS(i - 1).Frequency = 575 Then Findex = 2
            If form1.CELLS(i - 1).Frequency = 1724 Then Findex = 4
            If form1.CELLS(i - 1).Frequency = 1850 Then Findex = 3
            If form1.CELLS(i - 1).Frequency = 2850 Then Findex = 5
            If form1.CELLS(i - 1).Frequency = 3425 Then Findex = 6
            If form1.CELLS(i - 1).Frequency = 6375 Then Findex = 7


            ' CELL.pos(4) = New PointF(CELLS(i).pos(0).X + Size2 * Math.Sin(aci / 180 * Math.PI), CELLS(i).pos(0).Y - Size2 * Math.Cos(aci / 180 * Math.PI))
            NX = form1.CELLS(i - 1).pos(0).X + Size2 * Math.Sin(form1.CELLS(i - 1).Azimuth / 180 * Math.PI)
            NY = form1.CELLS(i - 1).pos(0).Y - Size2 * Math.Cos(form1.CELLS(i - 1).Azimuth / 180 * Math.PI)


            If form1.CELLS(i - 1).Azimuth >= 0 And form1.CELLS(i - 1).Azimuth <= 90 Then
                LBLX = 0 'LEN(PPP(Index).Tooltiptext)
                LBLY = -12 * (Findex)
            End If

            If form1.CELLS(i - 1).Azimuth > 90 And form1.CELLS(i - 1).Azimuth <= 180 Then
                LBLX = 0
                LBLY = 0 + 12 * (Findex)
            End If

            If form1.CELLS(i - 1).Azimuth > 180 And form1.CELLS(i - 1).Azimuth <= 270 Then
                LBLX = (-8) * Len(form1.CELLS(i - 1).Label)
                LBLY = 0 + 12 * (Findex)
            End If

            If form1.CELLS(i - 1).Azimuth > 270 And form1.CELLS(i - 1).Azimuth <= 359 Then
                LBLX = (-8) * Len(form1.CELLS(i - 1).Label)
                LBLY = -12 * (Findex)
            End If

            'form1.LabelLocations.Add(New Point(form1.CELLS(i - 1).pos(4).X, form1.CELLS(i - 1).pos(4).Y))
            'LabelLocations(i) = New Point((NX + CLng(LBLX)), NY + CLng(LBLY))
            form1.LabelLocations.Add(New Point(NX + CLng(LBLX), NY + CLng(LBLY)))
        Next



        form1.Refresh()
        form1.Invalidate()

        ' Display selected items (example: showing in a MessageBox)  form1.CELLDATA(i, Column)
        'MessageBox.Show("Selected items: " & String.Join(", ", selectedItems))
        'MessageBox.Show(selectedParameters1)
    End Sub

    Private Sub RemoveLabelButton_Click(sender As Object, e As EventArgs) Handles RemoveLabelButton.Click
        form1.LabelLocations.Clear()
        form1.ShowLabels = False
        form1.Invalidate()
        form1.ToolTipTextDefault()
        form1.Refresh()
    End Sub

    Private Sub ComplementaryCheckBox_Click(sender As Object, e As EventArgs) Handles ComplementaryCheckBox.Click
        HueCheckBox.Checked = False
        RandomCheckBox.Checked = False
        UnCheckAllRelationCheckBoxes()
    End Sub

    Private Sub HueCheckBox_Click(sender As Object, e As EventArgs) Handles HueCheckBox.Click
        ComplementaryCheckBox.Checked = False
        RandomCheckBox.Checked = False
        UnCheckAllRelationCheckBoxes()
    End Sub
    Private Sub RandomCheckBox_Click(sender As Object, e As EventArgs) Handles RandomCheckBox.Click
        ComplementaryCheckBox.Checked = False
        HueCheckBox.Checked = False
        UnCheckAllRelationCheckBoxes()
    End Sub
    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        PictureBox1.BorderStyle = BorderStyle.Fixed3D
        PictureBox2.BorderStyle = BorderStyle.FixedSingle
        PictureBox3.BorderStyle = BorderStyle.FixedSingle
        PictureBox4.BorderStyle = BorderStyle.FixedSingle
        PictureBox5.BorderStyle = BorderStyle.FixedSingle
        PictureBox6.BorderStyle = BorderStyle.FixedSingle
        form1.SelectedGradientColorIndex = 1
    End Sub
    Private Sub PictureBox2_Click(sender As Object, e As EventArgs) Handles PictureBox2.Click
        PictureBox1.BorderStyle = BorderStyle.FixedSingle
        PictureBox2.BorderStyle = BorderStyle.Fixed3D
        PictureBox3.BorderStyle = BorderStyle.FixedSingle
        PictureBox4.BorderStyle = BorderStyle.FixedSingle
        PictureBox5.BorderStyle = BorderStyle.FixedSingle
        PictureBox6.BorderStyle = BorderStyle.FixedSingle
        form1.SelectedGradientColorIndex = 2
    End Sub
    Private Sub PictureBox3_Click(sender As Object, e As EventArgs) Handles PictureBox3.Click
        PictureBox1.BorderStyle = BorderStyle.FixedSingle
        PictureBox2.BorderStyle = BorderStyle.FixedSingle
        PictureBox3.BorderStyle = BorderStyle.Fixed3D
        PictureBox4.BorderStyle = BorderStyle.FixedSingle
        PictureBox5.BorderStyle = BorderStyle.FixedSingle
        PictureBox6.BorderStyle = BorderStyle.FixedSingle
        form1.SelectedGradientColorIndex = 3
    End Sub
    Private Sub PictureBox4_Click(sender As Object, e As EventArgs) Handles PictureBox4.Click
        PictureBox1.BorderStyle = BorderStyle.FixedSingle
        PictureBox2.BorderStyle = BorderStyle.FixedSingle
        PictureBox3.BorderStyle = BorderStyle.FixedSingle
        PictureBox4.BorderStyle = BorderStyle.Fixed3D
        PictureBox5.BorderStyle = BorderStyle.FixedSingle
        PictureBox6.BorderStyle = BorderStyle.FixedSingle
        form1.SelectedGradientColorIndex = 4
    End Sub
    Private Sub PictureBox5_Click(sender As Object, e As EventArgs) Handles PictureBox5.Click
        PictureBox1.BorderStyle = BorderStyle.FixedSingle
        PictureBox2.BorderStyle = BorderStyle.FixedSingle
        PictureBox3.BorderStyle = BorderStyle.FixedSingle
        PictureBox4.BorderStyle = BorderStyle.FixedSingle
        PictureBox5.BorderStyle = BorderStyle.Fixed3D
        PictureBox6.BorderStyle = BorderStyle.FixedSingle
        form1.SelectedGradientColorIndex = 5
    End Sub
    Private Sub PictureBox6_Click(sender As Object, e As EventArgs) Handles PictureBox6.Click
        PictureBox1.BorderStyle = BorderStyle.FixedSingle
        PictureBox2.BorderStyle = BorderStyle.FixedSingle
        PictureBox3.BorderStyle = BorderStyle.FixedSingle
        PictureBox4.BorderStyle = BorderStyle.FixedSingle
        PictureBox5.BorderStyle = BorderStyle.FixedSingle
        PictureBox6.BorderStyle = BorderStyle.Fixed3D
        form1.SelectedGradientColorIndex = 6
    End Sub

    Private Sub OutgoingNeighborsCheckBox_Click(sender As Object, e As EventArgs) Handles OutgoingNeighborsCheckBox.Click
        'OutgoingNeighborsCheckBox.Checked = False
        IncomingNeighborsCheckBox.Checked = False
        PCIConflictCheckBox.Checked = False
        RSIConflictCheckBox.Checked = False
        PCIConfusionCheckBox.Checked = False
        UnCheckAllThematicColorCheckBoxes()

        If OutgoingNeighborsCheckBox.Checked = False Then CleanRelationTable()
        If OutgoingNeighborsCheckBox.Checked = True And form1.SelectedCellCount = 1 Then FillNeighborTable()
    End Sub

    Private Sub IncomingNeighborsCheckBox_Click(sender As Object, e As EventArgs) Handles IncomingNeighborsCheckBox.Click
        OutgoingNeighborsCheckBox.Checked = False
        'IncomingNeighborsCheckBox.Checked = False
        PCIConflictCheckBox.Checked = False
        RSIConflictCheckBox.Checked = False
        PCIConfusionCheckBox.Checked = False
        UnCheckAllThematicColorCheckBoxes()

        If IncomingNeighborsCheckBox.Checked = False Then CleanRelationTable()
        If IncomingNeighborsCheckBox.Checked = True And form1.SelectedCellCount = 1 Then FillIncNeighborTable()
    End Sub

    Private Sub PCIConflictCheckBox_Click(sender As Object, e As EventArgs) Handles PCIConflictCheckBox.Click
        OutgoingNeighborsCheckBox.Checked = False
        IncomingNeighborsCheckBox.Checked = False
        'PCIConflictCheckBox.Checked = False
        RSIConflictCheckBox.Checked = False
        PCIConfusionCheckBox.Checked = False
        UnCheckAllThematicColorCheckBoxes()

        If PCIConflictCheckBox.Checked = False Then CleanRelationTable()
        If PCIConflictCheckBox.Checked = True And form1.SelectedCellCount = 1 Then FillPCIConflictTable()

    End Sub

    Private Sub RSIConflictCheckBox_Click(sender As Object, e As EventArgs) Handles RSIConflictCheckBox.Click
        OutgoingNeighborsCheckBox.Checked = False
        IncomingNeighborsCheckBox.Checked = False
        PCIConflictCheckBox.Checked = False
        'RSIConflictCheckBox.Checked = False
        PCIConfusionCheckBox.Checked = False
        UnCheckAllThematicColorCheckBoxes()

        If RSIConflictCheckBox.Checked = False Then CleanRelationTable()
        If RSIConflictCheckBox.Checked = True And form1.SelectedCellCount = 1 Then FillRSIConflictTable()
    End Sub

    Private Sub PCIConfusionCheckBox_Click(sender As Object, e As EventArgs) Handles PCIConfusionCheckBox.Click
        OutgoingNeighborsCheckBox.Checked = False
        IncomingNeighborsCheckBox.Checked = False
        PCIConflictCheckBox.Checked = False
        RSIConflictCheckBox.Checked = False
        'PCIConfusionCheckBox.Checked = False
        UnCheckAllThematicColorCheckBoxes()

        If PCIConfusionCheckBox.Checked = False Then CleanRelationTable()
        If PCIConfusionCheckBox.Checked = True And form1.SelectedCellCount = 1 Then FillPCIConfusionTable()
    End Sub

    Private Sub OutgoingNeighborsCheckBox_CheckedChanged(sender As Object, e As EventArgs) Handles OutgoingNeighborsCheckBox.CheckedChanged
        If OutgoingNeighborsCheckBox.Checked = False Then CleanRelationTable()
        'If OutgoingNeighborsCheckBox.Checked = True And form1.SelectedCellCount = 1 Then FillNeighborTable()
    End Sub
    Private Sub IncomingNeighborsCheckBox_CheckedChanged(sender As Object, e As EventArgs) Handles IncomingNeighborsCheckBox.CheckedChanged
        If IncomingNeighborsCheckBox.Checked = False Then CleanRelationTable()
        'If IncomingNeighborsCheckBox.Checked = True And form1.SelectedCellCount = 1 Then FillIncNeighborTable()
    End Sub
    Private Sub PCIConflictCheckBox_CheckedChanged(sender As Object, e As EventArgs) Handles PCIConflictCheckBox.CheckedChanged
        If PCIConflictCheckBox.Checked = False Then CleanRelationTable()
        'If PCIConflictCheckBox.Checked = True And form1.SelectedCellCount = 1 Then FillPCIConflictTable()
    End Sub
    Private Sub PCIConfusionCheckBox_CheckedChanged(sender As Object, e As EventArgs) Handles PCIConfusionCheckBox.CheckedChanged
        If PCIConfusionCheckBox.Checked = False Then CleanRelationTable()
        If PCIConfusionCheckBox.Checked = False Then form1.ToolTipTextDefault()
        'If PCIConfusionCheckBox.Checked = True And form1.SelectedCellCount = 1 Then FillPCIConfusionTable()
    End Sub
    Private Sub RSIConflictCheckBox_CheckedChanged(sender As Object, e As EventArgs) Handles RSIConflictCheckBox.CheckedChanged
        If RSIConflictCheckBox.Checked = False Then CleanRelationTable()
        If RSIConflictCheckBox.Checked = False Then form1.ToolTipTextDefault()
        'If PCIConflictCheckBox.Checked = True And form1.SelectedCellCount = 1 Then FillPCIConflictTable()
    End Sub
    Public Sub UnCheckAllRelationCheckBoxes()
        If OutgoingNeighborsCheckBox.Checked = True Then OutgoingNeighborsCheckBox.Checked = False
        If IncomingNeighborsCheckBox.Checked = True Then IncomingNeighborsCheckBox.Checked = False
        If PCIConflictCheckBox.Checked = True Then PCIConflictCheckBox.Checked = False
        If RSIConflictCheckBox.Checked = True Then RSIConflictCheckBox.Checked = False
        If PCIConfusionCheckBox.Checked = True Then PCIConfusionCheckBox.Checked = False
        form1.ResetDrawing()
    End Sub

    Public Sub UnCheckAllThematicColorCheckBoxes()
        If ComplementaryCheckBox.Checked = True Then ComplementaryCheckBox.Checked = False
        If HueCheckBox.Checked = True Then HueCheckBox.Checked = False
        If RandomCheckBox.Checked = True Then RandomCheckBox.Checked = False
        ListView1.Clear()
        'ListView1.Refresh()
        form1.ResetDrawing()
    End Sub
    Public Sub UnCheckAllTACheckBoxes()

        If AVGTACheckBox.Checked = True Then AVGTACheckBox.Checked = False
        If PERCENTILETACheckBox.Checked = True Then PERCENTILETACheckBox.Checked = False
        'If MAXTACheckBox.Checked = True Then MAXTACheckBox.Checked = False
        'CleanTAChart()
        form1.ResetDrawing()
    End Sub
    Public Sub FillPCIConflictTable()
        RelationCount = 0
        RelationLineVisible = True
        Dim Distance_ As Double

        If PCIConflictCheckBox.Checked = True And form1.SelectedCellCount = 1 Then
            ListView2.Clear()
            'IsRelationTableFilled = False
            ListView2.View = View.Details
            ListView2.FullRowSelect = True
            ListView2.GridLines = True
            ListView2.Columns.Add("SCELL", 100, HorizontalAlignment.Left)
            'ListView2.Columns.Add("NECGI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("TCELL", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("SCELL-EARFCNDL", 120, HorizontalAlignment.Left)
            ListView2.Columns.Add("TCELL-EARFCNDL", 120, HorizontalAlignment.Left)
            ListView2.Columns.Add("SCELL-PCI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("TCELL-PCI", 100, HorizontalAlignment.Left)
            'ListView2.Columns.Add("CELL-RSI", 100, HorizontalAlignment.Left)
            'ListView2.Columns.Add("NCELL-RSI", 100, HorizontalAlignment.Left)
            'ListView2.Columns.Add("CELL-TAC", 100, HorizontalAlignment.Left)
            'ListView2.Columns.Add("NCELL-TAC", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("DISTANCE", 100, HorizontalAlignment.Left)

            For i = 0 To form1.CELLS.Length - 1
                If form1.CELLS(i).IsSelected = True Then
                    'SelectedCellNeiList = form1.CELLDATA(i + 1, form1.eutrancellrelationidclmn)
                Else
                    form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 255, 255))
                End If
            Next

            Dim Distance_Threshold As Double = 10000
            If IsNumeric(PCIConflictTextBox.Text) Then
                Distance_Threshold = PCIConflictTextBox.Text
            End If

            For i = 0 To form1.CELLS.Length - 1

                Distance_ = Math.Round(Math.Acos(Math.Sin(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Sin(form1.CELLS(i).Latitude * 3.14 / 180) + Math.Cos(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Longitude * 3.14 / 180 - form1.CELLS(form1.selectedCELLIndex).Longitude * 3.14 / 180)) * 6371, 2)

                If Distance_ <= Distance_Threshold And form1.CELLS(i).PCI = form1.CELLS(form1.selectedCELLIndex).PCI And form1.CELLS(i).Frequency = form1.CELLS(form1.selectedCELLIndex).Frequency And form1.CELLS(i).CellName <> form1.CELLS(form1.selectedCELLIndex).CellName Then
                    'MsgBox(form1.CELLS(i).CellName)
                    Dim item As New ListViewItem(form1.CELLS(form1.selectedCELLIndex).CellName)

                    item.SubItems.Add(form1.CELLS(i).CellName)
                    'item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).CellName) ' Consider if this duplication is needed
                    item.SubItems.Add(form1.CELLS(i).Frequency)
                    item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).Frequency)
                    item.SubItems.Add(form1.CELLS(i).PCI)
                    item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).PCI)
                    'item.SubItems.Add(form1.CELLS(i).RSI)
                    'item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).RSI)
                    'item.SubItems.Add(form1.CELLS(i).TAC)
                    'item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).TAC)


                    form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))



                    Dim Distance As Double = Math.Round(Math.Acos(Math.Sin(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Sin(form1.CELLS(i).Latitude * 3.14 / 180) + Math.Cos(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Longitude * 3.14 / 180 - form1.CELLS(form1.selectedCELLIndex).Longitude * 3.14 / 180)) * 6371, 2)
                    item.SubItems.Add(Distance_)
                    ListView2.Items.Add(item)

                    IsRelationTableFilled = True
                    RelationCount = RelationCount + 1
                    RelationCartesianCoordinates(RelationCount, 1) = i
                    RelationCartesianCoordinates(RelationCount, 2) = form1.CELLS(i).pos(0).X
                    RelationCartesianCoordinates(RelationCount, 3) = form1.CELLS(i).pos(0).Y
                    RelationCartesianCoordinates(RelationCount, 4) = form1.selectedCELLIndex
                    RelationCartesianCoordinates(RelationCount, 5) = form1.CELLS(form1.selectedCELLIndex).pos(0).X
                    RelationCartesianCoordinates(RelationCount, 6) = form1.CELLS(form1.selectedCELLIndex).pos(0).Y

                End If
            Next
            form1.Refresh()
        End If
    End Sub
    Public Sub FillRSIConflictTable()
        RelationCount = 0
        RelationLineVisible = True
        Dim Distance_ As Double

        If RSIConflictCheckBox.Checked = True And form1.SelectedCellCount = 1 Then
            ListView2.Clear()
            'IsRelationTableFilled = False
            ListView2.View = View.Details
            ListView2.FullRowSelect = True
            ListView2.GridLines = True
            ListView2.Columns.Add("SCELL", 100, HorizontalAlignment.Left)
            'ListView2.Columns.Add("NECGI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("TCELL", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("SCELL-EARFCNDL", 120, HorizontalAlignment.Left)
            ListView2.Columns.Add("TCELL-EARFCNDL", 120, HorizontalAlignment.Left)
            ListView2.Columns.Add("SCELL-RSI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("TCELL-RSI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("SCELL-CellRange", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("TCELL-CellRange", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("DISTANCE", 100, HorizontalAlignment.Left)

            For i = 0 To form1.CELLS.Length - 1
                If form1.CELLS(i).IsSelected = True Then
                    'SelectedCellNeiList = form1.CELLDATA(i + 1, form1.eutrancellrelationidclmn)
                Else
                    form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 255, 255))
                End If
            Next


            Dim Distance_Threshold As Double = 10000
            If IsNumeric(RSIConflictTextBox.Text) Then
                Distance_Threshold = RSIConflictTextBox.Text
            End If

            '#######################################################
            Dim cellrange1, cellrange2, RSI_Spr1, RSI_Spr2 As Double
            cellrange1 = form1.CELLS(form1.selectedCELLIndex).CELLRANGE
            If cellrange1 <= 0.76 Then RSI_Spr1 = 1
            If cellrange1 > 0.76 And cellrange1 <= 1.04 Then RSI_Spr1 = 2
            If cellrange1 > 1.04 And cellrange1 <= 1.47 Then RSI_Spr1 = 2
            If cellrange1 > 1.47 And cellrange1 <= 2.04 Then RSI_Spr1 = 2
            If cellrange1 > 2.04 And cellrange1 <= 2.62 Then RSI_Spr1 = 2
            If cellrange1 > 2.62 And cellrange1 <= 3.48 Then RSI_Spr1 = 3
            If cellrange1 > 3.48 And cellrange1 <= 4.33 Then RSI_Spr1 = 3
            If cellrange1 > 4.33 And cellrange1 <= 5.48 Then RSI_Spr1 = 4
            If cellrange1 > 5.48 And cellrange1 <= 7.34 Then RSI_Spr1 = 5
            If cellrange1 > 7.34 And cellrange1 <= 9.77 Then RSI_Spr1 = 6
            If cellrange1 > 9.77 And cellrange1 <= 12.2 Then RSI_Spr1 = 8
            If cellrange1 > 12.2 And cellrange1 <= 15.92 Then RSI_Spr1 = 10
            If cellrange1 > 15.92 And cellrange1 <= 22.78 Then RSI_Spr1 = 13
            If cellrange1 > 22.78 And cellrange1 <= 38.8 Then RSI_Spr1 = 22
            If cellrange1 > 38.8 And cellrange1 <= 68.83 Then RSI_Spr1 = 32
            If cellrange1 > 68.83 And cellrange1 <= 118.9 Then RSI_Spr1 = 64
            '#######################################################







            For i = 0 To form1.CELLS.Length - 1

                Distance_ = Math.Round(Math.Acos(Math.Sin(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Sin(form1.CELLS(i).Latitude * 3.14 / 180) + Math.Cos(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Longitude * 3.14 / 180 - form1.CELLS(form1.selectedCELLIndex).Longitude * 3.14 / 180)) * 6371, 2)

                If Distance_ <= Distance_Threshold And form1.CELLS(i).Frequency = form1.CELLS(form1.selectedCELLIndex).Frequency And form1.CELLS(i).CellName <> form1.CELLS(form1.selectedCELLIndex).CellName Then

                    '#######################################################
                    cellrange2 = form1.CELLS(i).CELLRANGE
                    If cellrange2 <= 0.76 Then RSI_Spr2 = 1
                    If cellrange2 > 0.76 And cellrange2 <= 1.04 Then RSI_Spr2 = 2
                    If cellrange2 > 1.04 And cellrange2 <= 1.47 Then RSI_Spr2 = 2
                    If cellrange2 > 1.47 And cellrange2 <= 2.04 Then RSI_Spr2 = 2
                    If cellrange2 > 2.04 And cellrange2 <= 2.62 Then RSI_Spr2 = 2
                    If cellrange2 > 2.62 And cellrange2 <= 3.48 Then RSI_Spr2 = 3
                    If cellrange2 > 3.48 And cellrange2 <= 4.33 Then RSI_Spr2 = 3
                    If cellrange2 > 4.33 And cellrange2 <= 5.48 Then RSI_Spr2 = 4
                    If cellrange2 > 5.48 And cellrange2 <= 7.34 Then RSI_Spr2 = 5
                    If cellrange2 > 7.34 And cellrange2 <= 9.77 Then RSI_Spr2 = 6
                    If cellrange2 > 9.77 And cellrange2 <= 12.2 Then RSI_Spr2 = 8
                    If cellrange2 > 12.2 And cellrange2 <= 15.92 Then RSI_Spr2 = 10
                    If cellrange2 > 15.92 And cellrange2 <= 22.78 Then RSI_Spr2 = 13
                    If cellrange2 > 22.78 And cellrange2 <= 38.8 Then RSI_Spr2 = 22
                    If cellrange2 > 38.8 And cellrange2 <= 68.83 Then RSI_Spr2 = 32
                    If cellrange2 > 68.83 And cellrange2 <= 118.9 Then RSI_Spr2 = 64
                    '#######################################################

                    If form1.CELLS(i).RSI = form1.CELLS(form1.selectedCELLIndex).RSI Or (form1.CELLS(form1.selectedCELLIndex).RSI > form1.CELLS(i).RSI And form1.CELLS(form1.selectedCELLIndex).RSI < form1.CELLS(i).RSI + RSI_Spr2) Or (form1.CELLS(form1.selectedCELLIndex).RSI < form1.CELLS(i).RSI And form1.CELLS(i).RSI < form1.CELLS(form1.selectedCELLIndex).RSI + RSI_Spr1) Then
                        'MsgBox(form1.CELLS(i).CellName)
                        Dim item As New ListViewItem(form1.CELLS(form1.selectedCELLIndex).CellName)

                        form1.CELLS(i).TooltipText = form1.CELLS(i).CellName & ":" & form1.CELLS(i).RSI


                        item.SubItems.Add(form1.CELLS(i).CellName)
                        'item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).CellName) ' Consider if this duplication is needed

                        item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).Frequency)
                        item.SubItems.Add(form1.CELLS(i).Frequency)
                        item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).RSI)
                        item.SubItems.Add(form1.CELLS(i).RSI)
                        item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).CELLRANGE)
                        item.SubItems.Add(form1.CELLS(i).CELLRANGE)

                        If form1.CELLS(i).RSI = form1.CELLS(form1.selectedCELLIndex).RSI Then
                            form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                        Else
                            form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 165, 0))
                        End If

                        item.SubItems.Add(Distance_)
                        ListView2.Items.Add(item)

                        IsRelationTableFilled = True
                        RelationCount = RelationCount + 1
                        RelationCartesianCoordinates(RelationCount, 1) = i
                        RelationCartesianCoordinates(RelationCount, 2) = form1.CELLS(i).pos(0).X
                        RelationCartesianCoordinates(RelationCount, 3) = form1.CELLS(i).pos(0).Y
                        RelationCartesianCoordinates(RelationCount, 4) = form1.selectedCELLIndex
                        RelationCartesianCoordinates(RelationCount, 5) = form1.CELLS(form1.selectedCELLIndex).pos(0).X
                        RelationCartesianCoordinates(RelationCount, 6) = form1.CELLS(form1.selectedCELLIndex).pos(0).Y

                    End If
                End If
            Next
            form1.Refresh()
        End If
    End Sub
    Public Sub FillPCIConfusionTable()
        Dim SelectedCellNeiList As String = ""
        Dim PCIConfusionArray(5000, 20) As String
        Dim Count As Integer = 0

        'RelationCartesianCoordinates(1, 1) = 0
        RelationCount = 0
        RelationLineVisible = True

        If PCIConfusionCheckBox.Checked = True And form1.SelectedCellCount = 1 Then

            ListView2.Clear()
            'IsRelationTableFilled = False
            ListView2.View = View.Details
            ListView2.FullRowSelect = True
            ListView2.GridLines = True
            ' Add column headers to ListView2
            ListView2.Columns.Add("CELL", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NECGI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-EARFCNDL", 120, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-EARFCNDL", 120, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-PCI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-PCI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-RSI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-RSI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-TAC", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-TAC", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("DISTANCE", 100, HorizontalAlignment.Left)
            'ListView2.CheckBoxes = True


            For i = 0 To form1.CELLS.Length - 1
                If form1.CELLS(i).IsSelected = True Then
                    SelectedCellNeiList = form1.CELLDATA(i + 1, form1.eutrancellrelationidclmn)
                    'form1.selectedCELLIndex = i
                Else
                    form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 255, 255))
                End If
            Next
            If SelectedCellNeiList.Length > 0 Then SelectedCellNeiList = SelectedCellNeiList.Substring(0, SelectedCellNeiList.Length - 1)
            Dim CellNeighbors() As String = Split(SelectedCellNeiList, ":")
            'MsgBox(SelectedCellNeiList)


            For j = 0 To UBound(CellNeighbors)
                For i = 0 To form1.CELLS.Length - 1
                    If form1.CELLS(i).CellName = CellNeighbors(j) Then
                        Count = Count + 1
                        PCIConfusionArray(Count, 1) = form1.CELLS(form1.selectedCELLIndex).CellName
                        PCIConfusionArray(Count, 2) = form1.CELLS(i).CellName
                        PCIConfusionArray(Count, 3) = form1.CELLS(i).CellName
                        PCIConfusionArray(Count, 4) = form1.CELLS(form1.selectedCELLIndex).Frequency
                        PCIConfusionArray(Count, 5) = form1.CELLS(i).Frequency
                        PCIConfusionArray(Count, 6) = form1.CELLS(form1.selectedCELLIndex).PCI
                        PCIConfusionArray(Count, 7) = form1.CELLS(i).PCI
                        PCIConfusionArray(Count, 8) = form1.CELLS(form1.selectedCELLIndex).RSI
                        PCIConfusionArray(Count, 9) = form1.CELLS(i).RSI
                        PCIConfusionArray(Count, 10) = form1.CELLS(form1.selectedCELLIndex).TAC
                        PCIConfusionArray(Count, 11) = form1.CELLS(i).TAC
                        Dim Distance As Double = Math.Round(Math.Acos(Math.Sin(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Sin(form1.CELLS(i).Latitude * 3.14 / 180) + Math.Cos(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Longitude * 3.14 / 180 - form1.CELLS(form1.selectedCELLIndex).Longitude * 3.14 / 180)) * 6371, 2)
                        PCIConfusionArray(Count, 12) = Distance
                        'form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                        PCIConfusionArray(Count, 19) = i
                    End If
                Next
            Next

            Dim PCI1, PCI2, Frequency1, Frequency2 As Integer
            Dim Cellname1, Cellname2 As String
            'MsgBox(Count)
            SelectedCellNeiList = ""
            If Count > 1 Then
                For i = 1 To Count
                    PCI1 = PCIConfusionArray(i, 7)
                    Frequency1 = PCIConfusionArray(i, 5)
                    Cellname1 = PCIConfusionArray(i, 3)
                    For j = 1 To Count
                        PCI2 = PCIConfusionArray(j, 7)
                        Frequency2 = PCIConfusionArray(j, 5)
                        Cellname2 = PCIConfusionArray(j, 3)
                        If PCI1 = PCI2 And Frequency1 = Frequency2 And Cellname1 <> Cellname2 Then
                            PCIConfusionArray(i, 20) = "OK"
                            PCIConfusionArray(j, 20) = "OK"
                            form1.CELLS(PCIConfusionArray(i, 19)).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                            form1.CELLS(PCIConfusionArray(i, 19)).TooltipText = form1.CELLS(PCIConfusionArray(i, 19)).CellName & ":" & form1.CELLS(PCIConfusionArray(i, 19)).PCI
                            form1.CELLS(PCIConfusionArray(j, 19)).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                            form1.CELLS(PCIConfusionArray(j, 19)).TooltipText = form1.CELLS(PCIConfusionArray(j, 19)).CellName & ":" & form1.CELLS(PCIConfusionArray(j, 19)).PCI
                            'MsgBox(Cellname1 & Cellname2)
                        End If
                    Next
                Next
            End If

            For i = 1 To Count
                If PCIConfusionArray(i, 20) = "OK" Then
                    Dim item As New ListViewItem(form1.CELLS(form1.selectedCELLIndex).CellName)

                    item.SubItems.Add(PCIConfusionArray(i, 2))
                    item.SubItems.Add(PCIConfusionArray(i, 3)) ' Consider if this duplication is needed
                    item.SubItems.Add(PCIConfusionArray(i, 4))
                    item.SubItems.Add(PCIConfusionArray(i, 5))
                    item.SubItems.Add(PCIConfusionArray(i, 6))
                    item.SubItems.Add(PCIConfusionArray(i, 7))
                    item.SubItems.Add(PCIConfusionArray(i, 8))
                    item.SubItems.Add(PCIConfusionArray(i, 9))
                    item.SubItems.Add(PCIConfusionArray(i, 10))
                    item.SubItems.Add(PCIConfusionArray(i, 11))
                    item.SubItems.Add(PCIConfusionArray(i, 12))


                    ' Move this line outside the inner loop
                    ListView2.Items.Add(item)

                    IsRelationTableFilled = True
                    RelationCount = RelationCount + 1
                    RelationCartesianCoordinates(RelationCount, 1) = form1.selectedCELLIndex
                    RelationCartesianCoordinates(RelationCount, 2) = form1.CELLS(form1.selectedCELLIndex).pos(0).X
                    RelationCartesianCoordinates(RelationCount, 3) = form1.CELLS(form1.selectedCELLIndex).pos(0).Y
                    RelationCartesianCoordinates(RelationCount, 4) = PCIConfusionArray(i, 19)
                    RelationCartesianCoordinates(RelationCount, 5) = form1.CELLS(PCIConfusionArray(i, 19)).pos(0).X
                    RelationCartesianCoordinates(RelationCount, 6) = form1.CELLS(PCIConfusionArray(i, 19)).pos(0).Y


                End If
            Next


        End If
        'form1.SelectedCellDetector()
        form1.Refresh()
    End Sub

    Public Sub FillIncNeighborTable()
        RelationCount = 0
        RelationLineVisible = True

        If IncomingNeighborsCheckBox.Checked = True And form1.SelectedCellCount = 1 Then
            ListView2.Clear()
            'IsRelationTableFilled = False
            ListView2.View = View.Details
            ListView2.FullRowSelect = True
            ListView2.GridLines = True
            ListView2.Columns.Add("CELL", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NECGI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-EARFCNDL", 120, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-EARFCNDL", 120, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-PCI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-PCI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-RSI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-RSI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-TAC", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-TAC", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("DISTANCE", 100, HorizontalAlignment.Left)

            For i = 0 To form1.CELLS.Length - 1
                If form1.CELLS(i).IsSelected = True Then
                    'SelectedCellNeiList = form1.CELLDATA(i + 1, form1.eutrancellrelationidclmn)
                Else
                    form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 255, 255))
                End If
            Next

            For i = 0 To form1.CELLS.Length - 1
                If InStr(form1.CELLDATA(i + 1, form1.eutrancellrelationidclmn), form1.CELLS(form1.selectedCELLIndex).CellName) Then
                    'MsgBox(form1.CELLS(i).CellName)
                    Dim item As New ListViewItem(form1.CELLS(i).CellName)

                    item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).CellName)
                    item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).CellName) ' Consider if this duplication is needed
                    item.SubItems.Add(form1.CELLS(i).Frequency)
                    item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).Frequency)
                    item.SubItems.Add(form1.CELLS(i).PCI)
                    item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).PCI)
                    item.SubItems.Add(form1.CELLS(i).RSI)
                    item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).RSI)
                    item.SubItems.Add(form1.CELLS(i).TAC)
                    item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).TAC)


                    form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(70, 70, 255))
                    If form1.CELLS(form1.selectedCELLIndex).Frequency <> form1.CELLS(i).Frequency Then
                        form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(0, 255, 255))
                    End If


                    Dim Distance As Double = Math.Round(Math.Acos(Math.Sin(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Sin(form1.CELLS(i).Latitude * 3.14 / 180) + Math.Cos(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Longitude * 3.14 / 180 - form1.CELLS(form1.selectedCELLIndex).Longitude * 3.14 / 180)) * 6371, 2)
                    item.SubItems.Add(Distance)
                    ListView2.Items.Add(item)

                    IsRelationTableFilled = True
                    RelationCount = RelationCount + 1
                    RelationCartesianCoordinates(RelationCount, 1) = i
                    RelationCartesianCoordinates(RelationCount, 2) = form1.CELLS(i).pos(0).X
                    RelationCartesianCoordinates(RelationCount, 3) = form1.CELLS(i).pos(0).Y
                    RelationCartesianCoordinates(RelationCount, 4) = form1.selectedCELLIndex
                    RelationCartesianCoordinates(RelationCount, 5) = form1.CELLS(form1.selectedCELLIndex).pos(0).X
                    RelationCartesianCoordinates(RelationCount, 6) = form1.CELLS(form1.selectedCELLIndex).pos(0).Y

                End If
            Next
            form1.Refresh()
        End If

    End Sub
    Public Sub FillNeighborTable()
        Dim SelectedCellNeiList As String = ""
        'RelationCartesianCoordinates(1, 1) = 0
        RelationCount = 0
        RelationLineVisible = True

        If OutgoingNeighborsCheckBox.Checked = True And form1.SelectedCellCount = 1 Then

            ListView2.Clear()
            'IsRelationTableFilled = False
            ListView2.View = View.Details
            ListView2.FullRowSelect = True
            ListView2.GridLines = True
            ' Add column headers to ListView2
            ListView2.Columns.Add("CELL", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NECGI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-EARFCNDL", 120, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-EARFCNDL", 120, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-PCI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-PCI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-RSI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-RSI", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("CELL-TAC", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("NCELL-TAC", 100, HorizontalAlignment.Left)
            ListView2.Columns.Add("DISTANCE", 100, HorizontalAlignment.Left)
            'ListView2.CheckBoxes = True


            For i = 0 To form1.CELLS.Length - 1
                If form1.CELLS(i).IsSelected = True Then
                    SelectedCellNeiList = form1.CELLDATA(i + 1, form1.eutrancellrelationidclmn)
                    'form1.selectedCELLIndex = i
                Else
                    form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 255, 255))
                End If
            Next
            If SelectedCellNeiList.Length > 0 Then SelectedCellNeiList = SelectedCellNeiList.Substring(0, SelectedCellNeiList.Length - 1)
            Dim CellNeighbors() As String = Split(SelectedCellNeiList, ":")
            'MsgBox(SelectedCellNeiList)
            For j = 0 To UBound(CellNeighbors)
                'form1.Distance.Text = form1.selectedCELLIndex
                Dim item As New ListViewItem(form1.CELLS(form1.selectedCELLIndex).CellName)
                For i = 0 To form1.CELLS.Length - 1
                    'form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 255, 255))
                    If form1.CELLS(form1.selectedCELLIndex).CellName <> form1.CELLS(i).CellName Then
                        'form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 255, 255))
                    End If
                    If form1.CELLS(i).CellName = CellNeighbors(j) Then


                        item.SubItems.Add(form1.CELLS(i).CellName)
                        item.SubItems.Add(form1.CELLS(i).CellName) ' Consider if this duplication is needed
                        item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).Frequency)
                        item.SubItems.Add(form1.CELLS(i).Frequency)
                        item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).PCI)
                        item.SubItems.Add(form1.CELLS(i).PCI)
                        item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).RSI)
                        item.SubItems.Add(form1.CELLS(i).RSI)
                        item.SubItems.Add(form1.CELLS(form1.selectedCELLIndex).TAC)
                        item.SubItems.Add(form1.CELLS(i).TAC)

                        form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(70, 70, 255))
                        If form1.CELLS(form1.selectedCELLIndex).Frequency <> form1.CELLS(i).Frequency Then
                            form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(0, 255, 255))
                        End If


                        'form1.CELLS(i).Latitude
                        'form1.CELLS(i).Longitude
                        'SIN(Lat1 * 3.14 / 180)*SIN(Lat2*3.14/180)+COS(Lat1*3.14/180)*COS(Lat2*3.14/180)*COS(Lon2*3.14/180-Lon1*3.14/180)
                        Dim Distance As Double = Math.Round(Math.Acos(Math.Sin(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Sin(form1.CELLS(i).Latitude * 3.14 / 180) + Math.Cos(form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Latitude * 3.14 / 180) * Math.Cos(form1.CELLS(i).Longitude * 3.14 / 180 - form1.CELLS(form1.selectedCELLIndex).Longitude * 3.14 / 180)) * 6371, 2)

                        item.SubItems.Add(Distance)

                        ' Move this line outside the inner loop
                        ListView2.Items.Add(item)
                        IsRelationTableFilled = True
                        RelationCount = RelationCount + 1
                        RelationCartesianCoordinates(RelationCount, 1) = form1.selectedCELLIndex
                        RelationCartesianCoordinates(RelationCount, 2) = form1.CELLS(form1.selectedCELLIndex).pos(0).X
                        RelationCartesianCoordinates(RelationCount, 3) = form1.CELLS(form1.selectedCELLIndex).pos(0).Y
                        RelationCartesianCoordinates(RelationCount, 4) = i
                        RelationCartesianCoordinates(RelationCount, 5) = form1.CELLS(i).pos(0).X
                        RelationCartesianCoordinates(RelationCount, 6) = form1.CELLS(i).pos(0).Y

                        Exit For ' Exit the inner loop as the match is found
                    Else
                        'form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 255, 255))
                    End If
                Next
            Next


        End If
        'form1.SelectedCellDetector()
        form1.Refresh()
    End Sub
    Public Sub RelationLineCoordinatesUpdate()
        Dim s_index, t_index As Integer
        'MsgBox(RelationCount)
        If RelationCount > 0 Then
            For i = 1 To RelationCount
                s_index = RelationCartesianCoordinates(i, 1)
                t_index = RelationCartesianCoordinates(i, 4)
                RelationCartesianCoordinates(i, 2) = form1.CELLS(s_index).pos(0).X
                RelationCartesianCoordinates(i, 3) = form1.CELLS(s_index).pos(0).Y
                RelationCartesianCoordinates(i, 5) = form1.CELLS(t_index).pos(0).X
                RelationCartesianCoordinates(i, 6) = form1.CELLS(t_index).pos(0).Y
            Next
        End If
    End Sub

    Public Sub CleanRelationTable()
        ListView2.Clear()
        IsRelationTableFilled = False
        RelationCount = 0
        RelationLineVisible = False

        For i = 0 To form1.CELLS.Length - 1
            If form1.CELLS(i).IsSelected = True Then

            Else
                form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(0, 255, 0))
            End If
        Next
        form1.Refresh()
    End Sub

    Private Sub TextBox1_Click(sender As Object, e As EventArgs) Handles PCIConflictTextBox.Click
        'PCIConflictTextBox.Text = ""
    End Sub


    Private Sub TextBox2_Click(sender As Object, e As EventArgs) Handles RSIConflictTextBox.Click
        RSIConflictTextBox.Text = ""
    End Sub

    Private Sub ComboBox2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox2O.SelectedIndexChanged
        If ComboBox2O.Text = "in" Then
            Label1.Text = "Seperator is comma ','"
        Else
            Label1.Text = ""
        End If
    End Sub
    Public Sub ComplementaryColors_()
        ' Declare and initialize the RGB array
        ComplementaryColors = New Integer(76, 2) {
                {255, 0, 0},  'Red
                {0, 255, 255},  'Cyan
                {0, 255, 0},  'Green
                {255, 0, 255},  'Magenta
                {0, 0, 255},  'Blue
                {255, 255, 0},  'Yellow
                {255, 165, 0},  'Orange
                {50, 205, 50},  'Lime
                {128, 0, 128},  'Purple
                {64, 224, 208},  'Turquoise
                {139, 0, 0},  'Dark Red
                {211, 211, 211},  'Light Gray
                {0, 0, 139},  'Dark Blue
                {255, 215, 0},  'Gold
                {0, 0, 128},  'Navy
                {255, 192, 203},  'Pink
                {0, 128, 128},  'Teal
                {128, 128, 0},  'Olive
                {0, 0, 0},  'Black
                {255, 255, 255},  'White
                {255, 0, 0},  'Red
                {0, 255, 255},  'Cyan
                {0, 255, 0},  'Green
                {255, 0, 255},  'Magenta
                {0, 0, 255},  'Blue
                {255, 255, 0},  'Yellow
                {255, 165, 0},  'Orange
                {128, 0, 128},  'Purple
                {191, 255, 0},  'Lime
                {0, 128, 128},  'Teal
                {255, 192, 203},  'Pink
                {128, 0, 0},  'Maroon
                {0, 0, 128},  'Navy
                {128, 128, 0},  'Olive
                {64, 64, 64},  'Dark Gray
                {192, 192, 192},  'Light Gray
                {255, 215, 0},  'Gold
                {0, 191, 255},  'Deep Sky Blue
                {220, 20, 60},  'Crimson
                {64, 224, 208},  'Turquoise
                {0, 100, 0},  'Dark Green
                {255, 140, 0},  'Dark Orange
                {238, 130, 238},  'Violet
                {34, 139, 34},  'Forest Green
                {255, 105, 180},  'Hot Pink
                {0, 0, 205},  'Medium Blue
                {0, 255, 127},  'Spring Green
                {127, 255, 0},  'Chartreuse
                {75, 0, 130},  'Indigo
                {210, 105, 30},  'Chocolate
                {250, 128, 114},  'Salmon
                {218, 112, 214},  'Orchid
                {199, 21, 133},  'Medium Violet Red
                {65, 105, 225},  'Royal Blue
                {255, 99, 71},  'Tomato
                {0, 206, 209},  'Dark Turquoise
                {240, 230, 140},  'Khaki
                {178, 34, 34},  'Firebrick
                {255, 20, 147},  'Deep Pink
                {139, 0, 0},  'Dark Red
                {106, 90, 205},  'Slate Blue
                {240, 128, 128},  'Light Coral
                {152, 251, 152},  'Pale Green
                {46, 139, 87},  'Sea Green
                {30, 144, 255},  'Dodger Blue
                {123, 104, 238},  'Medium Slate Blue
                {255, 127, 80},  'Coral
                {60, 179, 113},  'Medium Sea Green
                {221, 160, 221},  'Plum
                {112, 128, 144},  'Slate Gray
                {32, 178, 170},  'Light Sea Green
                {148, 0, 211},  'Dark Violet
                {186, 85, 211},  'Medium Orchid
                {160, 82, 45},  'Sienna
                {188, 143, 143},  'Rosy Brown
                {70, 130, 180},  'Steel Blue
                {139, 0, 139}  'Dark Magenta
            }
    End Sub

    Private Sub TooltipCheckBox_CheckedChanged(sender As Object, e As EventArgs) Handles TooltipCheckBox.CheckedChanged

    End Sub

    Private Sub AddParameterstoSelectedButton_Click(sender As Object, e As EventArgs) Handles AddParameterstoSelectedButtonO.Click
        Dim selectedItems As New List(Of String)
        Dim Column As Integer
        Dim SelectedCellParameters, Header As String
        SelectedTextO.Text = ""
        SelectedCellParameters = ""
        Header = "CellName" & vbTab

        For i = 1 To form1.CELLS.Length
            form1.CELLS(i - 1).Label = "" 'form1.CELLS(i - 1).CellName
        Next


        ' Loop through each selected item in the ListBox
        For Each item As String In LabelListBox.SelectedItems
            'selectedItems.Add(item) ' Add the selected item to the list
            Column = -1
            For i = 0 To form1.ParameterCount
                If item = form1.CELLDATA(0, i) Then Column = i
            Next
            Header = Header & form1.CELLDATA(0, Column) & vbTab
            'MsgBox(LabelListBox.SelectedItems.Count)
            If Column > -1 Then
                For i = 1 To form1.CELLS.Length
                    form1.CELLS(i - 1).Label = form1.CELLS(i - 1).Label & form1.CELLDATA(i, Column) & "|"
                Next
            End If
        Next

        Header = Header & vbCrLf

        For i = 1 To form1.CELLS.Length
            If form1.CELLS(i - 1).Label.Length > 0 Then form1.CELLS(i - 1).Label = form1.CELLS(i - 1).Label.Substring(0, form1.CELLS(i - 1).Label.Length - 1)
            If form1.CELLS(i - 1).IsSelected Then SelectedCellParameters = SelectedCellParameters & form1.CELLS(i - 1).CellName & vbTab & Replace(form1.CELLS(i - 1).Label, "|", vbTab) & vbCrLf
        Next

        SelectedTextO.Text = Header & SelectedCellParameters
    End Sub
    Private Sub Parse_TA_DATA_File(filePath As String)
        Dim InputRowCount As Integer = 0
        Try
            ' Clear the static array (optional: fill it with empty strings)
            For i As Integer = 0 To form1.TA_DATA.GetLength(0) - 1
                For j As Integer = 0 To form1.TA_DATA.GetLength(1) - 1
                    form1.TA_DATA(i, j, 0) = String.Empty
                Next
            Next

            Dim rowIndex As Integer = 0 ' To track the current row in TA_DATA_Temp

            ' Read the file line by line
            For Each line As String In File.ReadLines(filePath)
                ' Stop processing if the maximum rows are reached
                If rowIndex >= form1.TA_DATA.GetLength(0) Then Exit For

                ' Skip empty lines
                If String.IsNullOrWhiteSpace(line) Then Continue For

                ' Split the line by commas
                Dim values As String() = line.Split(","c)

                ' Process each column, respecting the max column limit
                For colIndex As Integer = 0 To Math.Min(values.Length - 1, form1.TA_DATA.GetLength(1) - 1)
                    form1.TA_DATA(rowIndex, colIndex, 0) = values(colIndex).Trim()
                Next

                rowIndex += 1
                InputRowCount = rowIndex
            Next

            'MsgBox(rowIndex)
            'MsgBox(colIndex)

            'MessageBox.Show("File parsed successfully!", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
        Catch ex As Exception
            'MessageBox.Show($"Error parsing file: {ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
        Dim count As Integer
        Dim TotalSample As Double
        count = 0
        For i As Integer = 0 To form1.CELLS.Length - 1
            TotalSample = 0
            For j As Integer = 0 To InputRowCount
                If form1.CELLS(i).CellName = form1.TA_DATA(j, 0, 0) Then
                    'TextBox1.Text = TextBox1.Text & form1.CELLS(i).CellName & vbCrLf
                    count = count + 1
                    For k = 0 To 55
                        If IsNumeric(form1.TA_DATA(j, 2 + k, 0)) Then
                            TotalSample = TotalSample + form1.TA_DATA(j, 2 + k, 0)
                            form1.TA_Location(i, k, 5) = form1.TA_DATA(j, 2 + k, 0)
                        End If
                    Next
                    If TotalSample > 0 Then
                        For k = 0 To 55
                            form1.TA_Location(i, k, 6) = 100 * form1.TA_Location(i, k, 5) / TotalSample
                        Next
                    End If
                End If
                GoTo nexti
nexti:
            Next
        Next
        'MsgBox(count)

        For i As Integer = 0 To form1.CELLS.Length - 1
            form1.CELLS(i).IsSelected = True
        Next
        form1.TA_XPERCENTILE()
        form1.DeSelectAll()

        'form1.TA_AVG()
        form1.TALOC()
        form1.TALOC_Cartesian()

    End Sub



    Private Sub Button1_Click(sender As Object, e As EventArgs)
        ' Open a file selection dialog
        Using openFileDialog As New OpenFileDialog
            openFileDialog.Filter = "CSV files (*.csv)|*.csv|All files (*.*)|*.*"
            openFileDialog.Title = "Select a CSV File"

            If openFileDialog.ShowDialog = DialogResult.OK Then
                ' Parse the selected file
                Parse_TA_DATA_File(openFileDialog.FileName)
            End If
        End Using




        If form1.SelectedCellCount = 1 Then
            'TextBox1.Text = form1.CELLS(form1.selectedCELLIndex).pos(0).X & "," & form1.CELLS(form1.selectedCELLIndex).pos(0).Y & vbCrLf
            'TextBox1.Text = TextBox1.Text & form1.CELLS(form1.selectedCELLIndex).pos(4).X & "," & form1.CELLS(form1.selectedCELLIndex).pos(4).Y & vbCrLf
            'TextBox1.Text = TextBox1.Text & form1.CELLS(form1.selectedCELLIndex).Latitude & "," & form1.CELLS(form1.selectedCELLIndex).Longitude & vbCrLf
            'TextBox1.Text = TextBox1.Text & form1.TA_Location(form1.selectedCELLIndex, 0, 1) & "," & form1.TA_Location(form1.selectedCELLIndex, 0, 2) & vbCrLf
            'TextBox1.Text = TextBox1.Text & form1.TA_Location_Cartesian(form1.selectedCELLIndex, 0, 1) & "," & form1.TA_Location_Cartesian(form1.selectedCELLIndex, 0, 2)
        End If

    End Sub
    Public Sub Show_Timing_Advance()
        'TextBox1.Text = ""
        'i inner, o outer 
        Dim outerRadius, innerRadius, Arc_c_x, Arc_c_y, Arc_i_x, Arc_i_y, Arc_o_x, Arc_o_y As Double
        Dim Arc_Index, r, g, b, Percentile As Integer
        'TextBox1.Text = ""
        'form1.TA_XPERCENTILE()
        form1.TALOC_Cartesian()
        Percentile = 0


        '####################################################SINGLE XPERCENTILE TIMING ADVANCE####################################################
        '#########################################################################################################################################
        If SingleCheckBox.Checked = True And PERCENTILETACheckBox.Checked = True And form1.SelectedCellCount = 1 Then
            Arc_c_x = form1.CELLS(form1.selectedCELLIndex).pos(0).X
            Arc_c_y = form1.CELLS(form1.selectedCELLIndex).pos(0).Y
            TextBox1.Text = form1.CELLS(form1.selectedCELLIndex).CellName & "-"
            'MsgBox(NumericUpDown1.Value)
            'MsgBox(form1.CELLS(form1.selectedCELLIndex).TA_XPerc_Dist_Index)
            For Arc_Index = 0 To form1.CELLS(form1.selectedCELLIndex).TA_XPerc_Dist_Index '34 'MaxArcIndex
                TextBox1.Text = TextBox1.Text & form1.TA_Location(form1.selectedCELLIndex, Arc_Index, 5) & "-"
                If form1.TA_Location(form1.selectedCELLIndex, Arc_Index, 8) > 0 Then
                    r = 255 - form1.TA_Location(form1.selectedCELLIndex, Arc_Index, 8) * 2.55
                    'If r > 200 And r <> 255 Then r = 200

                    g = r
                    b = 255

                    If Arc_Index = 0 Then Arc_i_x = form1.CELLS(form1.selectedCELLIndex).pos(0).X
                    If Arc_Index = 0 Then Arc_i_y = form1.CELLS(form1.selectedCELLIndex).pos(0).Y

                    If Arc_Index > 0 Then Arc_i_x = form1.TA_Location(form1.selectedCELLIndex, Arc_Index - 1, 4)
                    If Arc_Index > 0 Then Arc_i_y = form1.TA_Location(form1.selectedCELLIndex, Arc_Index - 1, 3)
                    Arc_o_x = form1.TA_Location(form1.selectedCELLIndex, Arc_Index, 4)
                    Arc_o_y = form1.TA_Location(form1.selectedCELLIndex, Arc_Index, 3)
                    outerRadius = Math.Sqrt((Arc_o_y - Arc_c_y) * (Arc_o_y - Arc_c_y) + (Arc_o_x - Arc_c_x) * (Arc_o_x - Arc_c_x))
                    innerRadius = Math.Sqrt((Arc_i_y - Arc_c_y) * (Arc_i_y - Arc_c_y) + (Arc_i_x - Arc_c_x) * (Arc_i_x - Arc_c_x))

                    Dim sweepAngle As Single = 66
                    Dim startAngle As Single = form1.CELLS(form1.selectedCELLIndex).Azimuth - 90 - (sweepAngle / 2) ' Center the arc at 90 degrees
                    Dim center As New Point(form1.CELLS(form1.selectedCELLIndex).pos(0).X, form1.CELLS(form1.selectedCELLIndex).pos(0).Y) ' Default center for all arcs
                    Dim color As Color = Color.FromArgb(255, r, g, b)
                    Dim newArc As New BlockArc(center, startAngle, sweepAngle, color, outerRadius, innerRadius)
                    form1.blockArcs.Add(newArc)
                    form1.Invalidate() ' Redraw the form to show the new arc
                End If
            Next

            If form1.blockArcs.Count > 0 Then form1.isPERCENTILETABlockArcVisible = True

            For i As Integer = 0 To 34
                TApdfCounter(i) = Math.Round(form1.TA_Location(form1.selectedCELLIndex, i, 6), 2)
            Next

            ' Update the chart with new values
            UpdateTAChart(TApdfCounter)

        End If
        '#########################################################################################################################################
        '#########################################################################################################################################


        '######################################################SINGLE AVERAGE TIMING ADVANCE######################################################
        '#########################################################################################################################################
        If SingleCheckBox.Checked = True And AVGTACheckBox.Checked = True And form1.SelectedCellCount = 1 Then
            'TextBox1.Text = form1.selectedCELLIndex
            If form1.CELLS(form1.selectedCELLIndex).TA_Avg_Dist > 0 Then
                'TextBox1.Text = TextBox1.Text & form1.CELLS(form1.selectedCELLIndex).TA_Avg_Dist
                r = 255
                g = 0
                b = 0

                Arc_c_x = form1.CELLS(form1.selectedCELLIndex).pos(0).X
                Arc_c_y = form1.CELLS(form1.selectedCELLIndex).pos(0).Y
                Arc_i_x = form1.CELLS(form1.selectedCELLIndex).pos(0).X
                Arc_i_y = form1.CELLS(form1.selectedCELLIndex).pos(0).Y
                Arc_o_x = form1.TA_Location(form1.selectedCELLIndex, 60, 4)
                Arc_o_y = form1.TA_Location(form1.selectedCELLIndex, 60, 3)
                outerRadius = Math.Sqrt((Arc_o_y - Arc_c_y) * (Arc_o_y - Arc_c_y) + (Arc_o_x - Arc_c_x) * (Arc_o_x - Arc_c_x))
                innerRadius = Math.Sqrt((Arc_i_y - Arc_c_y) * (Arc_i_y - Arc_c_y) + (Arc_i_x - Arc_c_x) * (Arc_i_x - Arc_c_x))
                Dim sweepAngle As Single = 7
                Dim startAngle As Single = form1.CELLS(form1.selectedCELLIndex).Azimuth - 90 - (sweepAngle / 2) ' Center the arc at 90 degrees
                Dim center As New Point(form1.CELLS(form1.selectedCELLIndex).pos(0).X, form1.CELLS(form1.selectedCELLIndex).pos(0).Y) ' Default center for all arcs
                Dim color As Color = Color.FromArgb(255, r, g, b)
                Dim newArc As New BlockArc(center, startAngle, sweepAngle, color, outerRadius, innerRadius)
                form1.blockArcs.Add(newArc)
                form1.Invalidate() ' Redraw the form to show the new arc
                If form1.blockArcs.Count > 0 Then form1.isAverageTABlockArcVisible = True
            End If
        End If
        '#########################################################################################################################################
        '#########################################################################################################################################

        '#####################################################MULTIPLE XPERCENTILE TIMING ADVANCE#################################################
        '#########################################################################################################################################
        If MultipleCheckBox.Checked = True And PERCENTILETACheckBox.Checked = True And form1.SelectedCellCount > 1 Then
            For i As Integer = 0 To form1.CELLS.Length - 1
                If form1.CELLS(i).TA_XPerc_Dist > 0 And form1.CELLS(i).IsSelected = True Then
                    'TextBox1.Text = form1.CELLS(form1.selectedCELLIndex).TA_Avg_Dist
                    r = 255
                    g = 0
                    b = 255

                    Arc_c_x = form1.CELLS(i).pos(0).X
                    Arc_c_y = form1.CELLS(i).pos(0).Y
                    Arc_i_x = form1.CELLS(i).pos(0).X
                    Arc_i_y = form1.CELLS(i).pos(0).Y
                    Arc_o_x = form1.TA_Location(i, 59, 4)
                    Arc_o_y = form1.TA_Location(i, 59, 3)
                    outerRadius = Math.Sqrt((Arc_o_y - Arc_c_y) * (Arc_o_y - Arc_c_y) + (Arc_o_x - Arc_c_x) * (Arc_o_x - Arc_c_x))
                    innerRadius = Math.Sqrt((Arc_i_y - Arc_c_y) * (Arc_i_y - Arc_c_y) + (Arc_i_x - Arc_c_x) * (Arc_i_x - Arc_c_x))
                    Dim sweepAngle As Single = 10
                    Dim startAngle As Single = form1.CELLS(i).Azimuth - 90 - (sweepAngle / 2) ' Center the arc at 90 degrees
                    Dim center As New Point(form1.CELLS(i).pos(0).X, form1.CELLS(i).pos(0).Y) ' Default center for all arcs
                    Dim color As Color = Color.FromArgb(128, r, g, b)
                    Dim newArc As New BlockArc(center, startAngle, sweepAngle, color, outerRadius, innerRadius)
                    form1.blockArcs.Add(newArc)
                    form1.Invalidate() ' Redraw the form to show the new arc
                    If form1.blockArcs.Count > 0 Then form1.isAverageTABlockArcVisible = True
                End If
            Next
        End If
        '#########################################################################################################################################
        '#########################################################################################################################################

        '#####################################################MULTIPLE AVERAGE TIMING ADVANCE#####################################################
        '#########################################################################################################################################
        If MultipleCheckBox.Checked = True And AVGTACheckBox.Checked = True And form1.SelectedCellCount > 1 Then
            For i As Integer = 0 To form1.CELLS.Length - 1
                If form1.CELLS(i).TA_Avg_Dist > 0 And form1.CELLS(i).IsSelected = True Then
                    'TextBox1.Text = form1.CELLS(form1.selectedCELLIndex).TA_Avg_Dist
                    r = 255
                    g = 0
                    b = 0

                    Arc_c_x = form1.CELLS(i).pos(0).X
                    Arc_c_y = form1.CELLS(i).pos(0).Y
                    Arc_i_x = form1.CELLS(i).pos(0).X
                    Arc_i_y = form1.CELLS(i).pos(0).Y
                    Arc_o_x = form1.TA_Location(i, 60, 4)
                    Arc_o_y = form1.TA_Location(i, 60, 3)
                    outerRadius = Math.Sqrt((Arc_o_y - Arc_c_y) * (Arc_o_y - Arc_c_y) + (Arc_o_x - Arc_c_x) * (Arc_o_x - Arc_c_x))
                    innerRadius = Math.Sqrt((Arc_i_y - Arc_c_y) * (Arc_i_y - Arc_c_y) + (Arc_i_x - Arc_c_x) * (Arc_i_x - Arc_c_x))
                    Dim sweepAngle As Single = 7
                    Dim startAngle As Single = form1.CELLS(i).Azimuth - 90 - (sweepAngle / 2) ' Center the arc at 90 degrees
                    Dim center As New Point(form1.CELLS(i).pos(0).X, form1.CELLS(i).pos(0).Y) ' Default center for all arcs
                    Dim color As Color = Color.FromArgb(128, r, g, b)
                    Dim newArc As New BlockArc(center, startAngle, sweepAngle, color, outerRadius, innerRadius)
                    form1.blockArcs.Add(newArc)
                    form1.Invalidate() ' Redraw the form to show the new arc
                    If form1.blockArcs.Count > 0 Then form1.isPERCENTILETABlockArcVisible = True
                End If
            Next
        End If
        '#########################################################################################################################################
        '#########################################################################################################################################



    End Sub
    Private Sub NumericUpDown1_MouseClick(sender As Object, e As MouseEventArgs) Handles NumericUpDown1.MouseClick

    End Sub
    Private Sub NumericUpDown1_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDown1.ValueChanged

        If PERCENTILETACheckBox.Checked = True Then
            CleanAVG_PERCENTILETABlockARCs()
            form1.TA_XPERCENTILE()
            Show_Timing_Advance()
        End If
        'If NumericUpDown1.Value > NumericUpDown2.Value Then NumericUpDown1.Value = NumericUpDown2.Value
    End Sub
    Private Sub MultipleCheckBox_CheckedChanged(sender As Object, e As EventArgs) Handles MultipleCheckBox.CheckedChanged
        If MultipleCheckBox.Checked = False Then
            If AVGTACheckBox.Checked = True Then AVGTACheckBox.Checked = False
            If PERCENTILETACheckBox.Checked = True Then PERCENTILETACheckBox.Checked = False
        End If
    End Sub

    Private Sub MultipleCheckBox_Click(sender As Object, e As EventArgs) Handles MultipleCheckBox.Click
        If MultipleCheckBox.Checked = True Then SingleCheckBox.Checked = False
    End Sub


    Private Sub SingleCheckBox_Click(sender As Object, e As EventArgs) Handles SingleCheckBox.Click
        If SingleCheckBox.Checked = True Then MultipleCheckBox.Checked = False
        FillTAComparisonTable()
    End Sub
    Private Sub SingleCheckBox_CheckedChanged(sender As Object, e As EventArgs) Handles SingleCheckBox.CheckedChanged
        If SingleCheckBox.Checked = False Then
            If AVGTACheckBox.Checked = True Then AVGTACheckBox.Checked = False
            If PERCENTILETACheckBox.Checked = True Then PERCENTILETACheckBox.Checked = False
        End If
        FillTAComparisonTable()
    End Sub

    Private Sub AVGTACheckBox_CheckedChanged(sender As Object, e As EventArgs) Handles AVGTACheckBox.CheckedChanged
        If AVGTACheckBox.Checked = False Then 'And form1.isAverageTABlockArcVisible = True
            CleanAVG_PERCENTILETABlockARCs()
        End If
        If PERCENTILETACheckBox.Checked = True Then Show_Timing_Advance()
        If AVGTACheckBox.Checked = True Then Show_Timing_Advance()
    End Sub

    Private Sub PERCENTILETACheckBox_CheckedChanged(sender As Object, e As EventArgs) Handles PERCENTILETACheckBox.CheckedChanged
        If PERCENTILETACheckBox.Checked = False Then 'And form1.isPERCENTILETABlockArcVisible = True
            CleanAVG_PERCENTILETABlockARCs()
        End If
        If PERCENTILETACheckBox.Checked = True Then
            form1.TA_XPERCENTILE()
            Show_Timing_Advance()
        End If
        If AVGTACheckBox.Checked = True Then Show_Timing_Advance()
    End Sub
    Public Sub CleanAVG_PERCENTILETABlockARCs()
        form1.blockArcs.Clear()
        form1.Invalidate()
        form1.isPERCENTILETABlockArcVisible = False
        form1.isAverageTABlockArcVisible = False
        CleanTAChart()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        ' Open a file selection dialog
        Using openFileDialog As New OpenFileDialog()
            openFileDialog.Filter = "CSV files (*.csv)|*.csv|All files (*.*)|*.*"
            openFileDialog.Title = "Select a CSV File"

            If openFileDialog.ShowDialog() = DialogResult.OK Then
                ' Parse the selected file
                Parse_TA_DATA_File(openFileDialog.FileName)
            End If
        End Using
    End Sub

    ' Configure the Chart control
    Private Sub ConfigureTAChart()
        ' Set up the chart area
        Dim chartArea As New ChartArea("ChartArea1")
        'Chart1.ChartAreas.Add(chartArea)
        'Chart1.Series.Clear(Series1)

        ' Set up the series
        Dim series As New Series("TA Distribution")
        series.ChartType = SeriesChartType.Column
        TA_CHART.Series.Add(series)

        ' Set axis titles
        TA_CHART.ChartAreas("ChartArea1").AxisX.Title = "Index (0 to 34)"
        TA_CHART.ChartAreas("ChartArea1").AxisY.Title = "Value"
    End Sub

    ' Update the chart with new data
    Private Sub UpdateTAChart(values As Double())
        ' Clear existing points
        TA_CHART.Series("TA Distribution").Points.Clear()


        Dim DistanceArray As Double() = {
            0.08, 0.23, 0.63, 1.01, 1.64, 2.42, 3.36, 4.45, 5.7, 7.18, 8.74, 10.5,
            12.5, 14.6, 16.9, 19.4, 22, 24.8, 27.9, 31.1, 34.5, 38.1, 41.8, 45.8,
            50, 54.3, 58.9, 63.5, 68.5, 73.6, 78.9, 84.5, 90.2, 96.1, 100
        }


        ' Add new points
        For i As Integer = 0 To values.Length - 1
            'Chart1.Series("TA Distribution").Points.AddXY(i, values(i))

            Dim point As DataPoint = New DataPoint(i, values(i))
            point.ToolTip = $"Distance(km): {DistanceArray(i)}, Perc%: {values(i)}" ' Tooltip for each column
            TA_CHART.Series("TA Distribution").Points.Add(point)
        Next
    End Sub
    Private Sub CleanTAChart()
        TA_CHART.Series("TA Distribution").Points.Clear()
    End Sub

    Public Sub FillTAComparisonTable()
        TA_ListView.Clear()
        'IsRelationTableFilled = False
        TA_ListView.View = View.Details
        TA_ListView.FullRowSelect = True
        TA_ListView.GridLines = True
        ' Add column headers to 
        TA_ListView.Columns.Add("CELL", 40, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("Avg TA", 50, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("X% TA", 50, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("BAND", 50, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("W/MHz", 60, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("CrsGain", 60, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("Tilt", 40, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("CellRange", 70, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("AntGain", 60, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("AntHei", 50, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("Altitude", 60, HorizontalAlignment.Left)
        TA_ListView.Columns.Add("TX/RX", 50, HorizontalAlignment.Left)
    End Sub

    Private Sub CalcIncOvershooterButton_Click(sender As Object, e As EventArgs) Handles CalcIncOvershooterButton.Click
        Dim Lat1, Lon1, Azi1, Lat2, Lon2, Azi2, Distance, Bearing, DeltaBearing As Double
        Dim EricssonTAIndex As Double() = {
            0.08, 0.23, 0.63, 1.01, 1.64, 2.42, 3.36, 4.45, 5.7, 7.18, 8.74, 10.5,
            12.5, 14.6, 16.9, 19.4, 22, 24.8, 27.9, 31.1, 34.5, 38.1, 41.8, 45.8,
            50, 54.3, 58.9, 63.5, 68.5, 73.6, 78.9, 84.5, 90.2, 96.1, 100
        }
        'TA_XPERCENTILE()

        TextBox1.Text = ""
        If form1.SelectedCellCount > 0 And form1.selectedCELLIndex <> -1 Then
            Lat1 = form1.CELLS(form1.selectedCELLIndex).Latitude * 3.14 / 180
            Lon1 = form1.CELLS(form1.selectedCELLIndex).Longitude * 3.14 / 180
            Azi1 = form1.CELLS(form1.selectedCELLIndex).Azimuth


            For i = 0 To form1.CELLS.Length - 1
                Lat2 = form1.CELLS(i).Latitude * 3.14 / 180
                Lon2 = form1.CELLS(i).Longitude * 3.14 / 180
                Azi2 = form1.CELLS(i).Azimuth

                Dim φ1 As Double = (Lat2)
                Dim φ2 As Double = (Lat1)
                Dim Δλ As Double = (Lon1 - Lon2)
                Dim y As Double = Math.Sin(Δλ) * Math.Cos(φ2)
                Dim x As Double = Math.Cos(φ1) * Math.Sin(φ2) - Math.Sin(φ1) * Math.Cos(φ2) * Math.Cos(Δλ)
                Dim θ As Double = Math.Atan2(y, x)
                Bearing = ((θ) * 180 / 3.14 + 360) Mod 360

                'bearing = (Math.Atan2(Math.Cos(Lat1) * Math.Sin(Lat2) - Math.Sin(Lat1) * Math.Cos(Lat2) * Math.Cos(Lon2 - Lon1), Math.Sin(Lon2 - Lon1) * Math.Cos(Lat2))) * 180 / 3.14

                'Azi1 = Azi1 * 180 / 3.14
                'Azi2 = Azi2 * 180 / 3.14
                If Math.Abs(Bearing - Azi2) <= 180 Then DeltaBearing = Math.Abs(Bearing - Azi2)
                If Math.Abs(Bearing - Azi2) > 180 Then DeltaBearing = 360 - Math.Abs(Bearing - Azi2)

                If Math.Abs(DeltaBearing) < 33 Then
                    'If Math.Abs(Bearing) < 270 Then   'form1.CELLS(i).TA_XPerc_Dist_Index  EricssonTAIndex(form1.CELLS(i).TA_XPerc_Dist_Index)

                    Distance = Math.Acos(Math.Sin(Lat1) * Math.Sin(Lat2) + Math.Cos(Lat1) * Math.Cos(Lat2) * Math.Cos(Lon2 - Lon1)) * 6371
                    If Distance < form1.CELLS(i).TA_XPerc_Dist And Distance > 0.01 Then
                        'If Distance < form1.CELLS(form1.selectedCELLIndex).TA_XPerc_Dist And Distance > 0.01 Then
                        'If Distance < EricssonTAIndex(form1.CELLS(form1.selectedCELLIndex).TA_XPerc_Dist_Index) And Distance > 0.01 Then

                        TextBox1.Text = TextBox1.Text & form1.CELLS(i).CellName & "-" & form1.CELLS(i).TA_XPerc_Dist & "-" & Bearing & vbCrLf
                        form1.CELLS(i).IsSelected = True
                        form1.CELLS(i).FillColor = New SolidBrush(Color.FromArgb(255, 0, 0))
                        form1.CELLS(i).BorderColor = New Pen(Color.Black, 2)
                    End If
                End If

            Next
        End If

        form1.SelectedCellDetector()
        form1.Refresh()
    End Sub
End Class
