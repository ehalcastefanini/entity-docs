<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustSalesMan` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcustSalesMan` code unit defines a frame (`TFRAMEcustSalesMan`) that provides a grid-based interface for managing and editing salesmen data associated with customers. It allows users to view, add, and edit salesmen information in a structured grid format. The main objective is to streamline the management of salesmen data by providing a user-friendly interface with customizable grid settings and actions.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **SOAP Services**: For interacting with external services (e.g., `SalesManServiceUtils`).
- **Database Components**: For managing and displaying data from a database (`cxDBData`, `DBClient`).
- **Custom Grid Components**: `cxGrid`, `cxGridDBTableView` for displaying and editing data in a tabular format.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **businessUnit**: Read-only.
2. **formatOrc**: Read-only.
3. **salesmanName**: Read-only.
4. **salesman**: Editable with a custom editor (`cxEDTfind`).

#### Grid Actions and Their Effects:
1. **Add Action**: Adds a new salesman entry.
2. **Edit Action**: Allows editing of existing salesman data.
3. **Apply Action**: Saves changes made to the grid.
4. **Cancel Action**: Cancels any unsaved changes.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Add Salesman**: Triggered by the `ACTaddExecute` procedure.
2. **Edit Salesman**: Triggered by the `cxDBVtableEditValueChanged` event.
3. **Search Salesman**: Triggered by the `m_SetFindSalesMan` procedure.

### Main Components:
- **Grid (`cxGrid`)**: Displays the salesmen data.
- **Custom Editor (`cxEDTfind`)**: Provides a search functionality for selecting salesmen.
- **Action Buttons**: Add, Apply, and Cancel buttons for managing grid actions.

### Pseudo-code for Actions and Events:
- `OnClick` event of Add Button: `if add button clicked then execute ACTaddExecute`.
- `OnEditValueChanged` event of grid: `if grid cell value changed then execute cxDBVtableEditValueChanged`.
- `OnButtonClick` event of custom editor: `if search button clicked then execute m_SetFindSalesMan`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized in the `Create` constructor.
   - Grid settings are configured (hidden fields, read-only fields, order fields, and custom editors).
   - Action panel visibility and available actions are set.

2. **User Interactions**:
   - Clicking the "Add" button triggers the `ACTaddExecute` procedure.
   - Editing a grid cell triggers the `cxDBVtableEditValueChanged` event.
   - Clicking the search button in the custom editor triggers the `m_SetFindSalesMan` procedure.

### Functions and File Locations:
1. **`ACTaddExecute`**: Adds a new salesman entry.
2. **`cxDBVtableEditValueChanged`**: Handles changes in grid cell values.
3. **`m_SetFindSalesMan`**: Opens a search dialog for selecting a salesman.

### Data Input:
- Users must provide or edit the following fields:
  - `salesman`: Editable field with a custom search editor.
  - `salesmanName`: Automatically populated based on the selected salesman.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Action**:
   - Preconditions: None.
   - Action: Adds a new row to the grid for entering salesman data.
2. **Edit Action**:
   - Preconditions: A row must be selected.
   - Action: Allows editing of the selected row.
3. **Apply Action**:
   - Preconditions: Changes must be made to the grid.
   - Action: Saves changes to the database.
4. **Cancel Action**:
   - Preconditions: Changes must be made to the grid.
   - Action: Discards unsaved changes.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- `salesman`: Uses a custom editor (`cxEDTfind`) for validation and selection.
- Other validations are not explicitly defined in the code.

---

## 5. Main Functions:

1. **`Create` Constructor**:
   - Initializes the frame and configures grid settings.
2. **`ACTaddExecute`**:
   - Adds a new salesman entry.
3. **`cxDBVtableEditValueChanged`**:
   - Handles changes in grid cell values.
4. **`m_SetFindSalesMan`**:
   - Opens a search dialog for selecting a salesman.

---

## 6. API Service Consumption:

- **Service Name**: SalesManServiceUtils.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Interacts with external services for salesman-related operations.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Custom Editor (`cxEDTfind`)**:
  - Appears only when editing the `salesman` field.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP service interactions.
- **cxGrid**: For grid-based UI components.
- **DBClient**: For database operations.

### Custom Components:
- **`cxEDTfind`**: Custom editor for searching and selecting salesmen.

---

## 9. Fields and Validations Listing:

1. **businessUnit**:
   - Type: String.
   - Read-only.
2. **formatOrc**:
   - Type: String.
   - Read-only.
3. **salesmanName**:
   - Type: String.
   - Read-only.
4. **salesman**:
   - Type: String.
   - Editable with a custom editor (`cxEDTfind`).

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEcustSalesMan.ACTaddExecute(Sender: TObject);
begin
  // Code to add a new salesman entry
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Settings**:
  - Hidden fields: `'HIDE_ALL_FIELDS'`.
  - Read-only fields: `'businessUnit;formatOrc;salesmanName'`.
  - Order fields: `'businessUnit;formatOrc;salesman;salesmanName'`.
- **Custom Editor**:
  - `cxEDTfind` is used for the `salesman` field.

---

## 12. Conclusion:

The `FRcustSalesMan` code unit provides a robust and customizable grid interface for managing salesmen data. Its strengths include the use of a custom editor for searching and selecting salesmen and the ability to configure grid settings dynamically. However, the lack of explicit error handling and validation logic is a limitation.

---

## 13. Short Summary:

The `FRcustSalesMan` unit implements a grid-based interface for managing salesmen data, featuring customizable grid settings and a custom search editor. It supports adding, editing, and saving data, with SOAP service integration for external operations.#### **FRcustSalesMan.pas**

```
unit FRcustSalesMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, kneTypes;

type
  TFRAMEcustSalesMan = class(TFRAMEBaseGridEditSOA)
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  private
    { Private declarations }
    procedure m_SetFindSalesMan(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetFindByCodeSalesMan(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

  end;

var
  FRAMEcustSalesMan: TFRAMEcustSalesMan;

implementation

uses
  kneUtils, kneFindDialog, kneDialogFactory, kneConfigObjects, kneFGFindUtils,
  FRfindCriteriaSalesMan,
  SalesManServiceUtils;

{$R *.dfm}

constructor TFRAMEcustSalesMan.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=customerCode';
  DataPacketName := 'SalesmanByProd';
  PropertyName := 'salesByProd';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := '';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Hidden Fields ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Read-Only Fields ........................................................
    DefineReadOnlyFields('businessUnit;formatOrc;salesmanName');  // [14-10-2015, #22272]
    // Order Fields ............................................................
    DefineOrderFields('businessUnit;formatOrc;salesman;salesmanName'); // [14-10-2015, #22272]
		// Custom Editors ..........................................................
    AddCustomField('salesman','cxEDTfind');

  end; //with

  cxEDTfind.Properties.OnButtonClick := m_SetFindSalesMan;
end;


procedure TFRAMEcustSalesMan.m_SetFindSalesMan(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
begin

  try
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    with lv_Find do
    begin
      with Options.DataSelection do
      begin
        FieldNameForCode := 'salesMan';
        FieldNamesForDesc.Clear;
        FieldNamesForDesc.Add('name');

        TargetDataSet := cxDBVtable.DataController.DataSource.DataSet;
        TargetFieldNameForCode := 'salesman';
        TargetFieldNamesForDesc.Clear;
        TargetFieldNamesForDesc.Add('salesmanName');

        UseTargetDataSet:= True;
```

#### **FRcustSalesMan.dfm**

```
inherited FRAMEcustSalesMan: TFRAMEcustSalesMan
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Left = 2
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
```
<!-- tabs:end -->

