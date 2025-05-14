<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRwarehouseMills` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRwarehouseMills` code unit defines a frame (`TFRAMEwarehouseMills`) that manages a grid-based interface for handling warehouse and mill data. It provides functionalities for adding, editing, and validating records related to mills and warehouses. The main objective is to allow users to manage the relationship between mills and warehouses efficiently, ensuring data integrity and providing a user-friendly interface for interaction.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services (e.g., `MillServiceUtils`).
- **Database Components**: For managing and validating data (`TClientDataSet`, `DB`).
- **cxGrid**: A grid component for displaying and editing data.
- **Custom Components**: Includes custom editors and find dialogs.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **mill**: Custom editor (`cxEDTfind`).
2. **millWhse**: Custom editor (`cxEDTmillWhse`).
3. **sapCode**: Standard field.
4. **stat**: Custom editor (`cxEDTstat`).

#### Grid Actions and Their Effects:
1. **Add**: Adds a new record to the grid.
2. **Delete**: Deletes the selected record from the grid.
3. **Edit**: Allows editing of existing records in the grid.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new mill-warehouse record.
2. Edit existing records.
3. Delete records.
4. Validate data before saving.
5. Search for mills using a custom find dialog.

### Main Components:
- **Grid (`cxGrid`)**: Displays the mill-warehouse data.
- **Custom Editors**: Provides specialized input fields for certain columns.
- **Action Panel**: Contains buttons for actions like Add and Delete.
- **Find Dialog**: Allows users to search for mills.

### Pseudo-code for Actions and Events:
- **OnEditValueChanged**: `if grid cell value changed then validate and update record`.
- **OnButtonClick (Find Dialog)**: `if find button clicked then open find dialog and return selected value`.
- **BeforePost**: `if dataset is about to save then validate data`.
- **Add Button Click**: `if add button clicked then create new record`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created (`Create` constructor).
   - Grid settings are configured (e.g., hidden fields, column order, key fields).
   - Action panel and available actions are set up.
   - Event handlers are assigned to custom editors.

2. **User Interactions**:
   - **Add Record**: User clicks "Add," triggering the `ACTaddExecute` procedure.
   - **Edit Record**: User modifies a cell, triggering `cxDBVtableEditValueChanged`.
   - **Delete Record**: User clicks "Delete," removing the selected record.
   - **Find Mill**: User clicks the find button, opening a dialog to search for mills.

3. **Functions**:
   - `Create` (File: `FRwarehouseMills.pas`): Initializes the frame and configures settings.
   - `m_FindMill` (File: `FRwarehouseMills.pas`): Opens the find dialog for mills.
   - `m_Validate` (File: `FRwarehouseMills.pas`): Validates the data before saving.

### Required Data:
- **Mill**: The mill code.
- **MillWhse**: The warehouse code.
- **SAP Code**: The SAP code for the mill-warehouse relationship.
- **Status**: The status of the record.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add**: Enabled at all times.
2. **Delete**: Enabled only when a record is selected.
3. **Edit**: Enabled for editable fields.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Duplicate record" if a record with the same key fields already exists.
- "Invalid data" if validation fails during `BeforePost`.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **Mill**: Must be a valid mill code (validated via `m_FindMill`).
- **MillWhse**: Must be a valid warehouse code.
- **Status**: Must be a valid status value.

---

## 5. Main Functions:

1. **`Create`**: Initializes the frame and configures grid settings.
2. **`m_FindMill`**: Opens a find dialog to search for mills.
3. **`m_Validate`**: Validates the data before saving.
4. **`SetMillsWhseDesc`**: Updates the description of a mill-warehouse relationship.

---

## 6. API Service Consumption:

- **Service Name**: `MillServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Mill and warehouse codes for validation or search.
- **Data Received**: Mill details or validation results.
- **Purpose**: To validate or search for mills.
- **Error Handling**: If the service fails, an error message is displayed.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid display and interaction.
- **SOAPHTTPClient**: For SOAP service communication.
- **DBClient**: For managing datasets.

### Custom Components:
- **TFRAMEBaseGridEditSOA**: Base class for the frame.
- **TMillServiceUtils**: Utility for interacting with mill-related services.

---

## 9. Fields and Validations Listing:

1. **Mill**:
   - Type: String.
   - Required: Yes.
   - Validation: Must be a valid mill code.
2. **MillWhse**:
   - Type: String.
   - Required: Yes.
   - Validation: Must be a valid warehouse code.
3. **SAP Code**:
   - Type: String.
   - Required: No.
4. **Status**:
   - Type: String.
   - Required: Yes.
   - Validation: Must be a valid status value.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEwarehouseMills.ACTaddExecute(Sender: TObject);
begin
  // Add a new record
  CDStable.Append;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Settings**: Configures hidden fields, column order, and key fields.
- **Custom Editors**: Assigns custom editors to specific columns.
- **Find Dialog**: Provides a mechanism for searching mills.

---

## 12. Conclusion:

The `FRwarehouseMills` code unit provides a robust framework for managing mill-warehouse relationships. It leverages a grid-based interface, custom editors, and SOAP services to ensure data integrity and user-friendly interaction. However, the lack of explicit error handling and default values for some fields may require additional implementation.

---

## 13. Short Summary:

The `FRwarehouseMills` unit manages mill-warehouse relationships using a grid interface, custom editors, and SOAP services. It supports adding, editing, and validating records, ensuring data integrity and user-friendly interaction.#### **FRwarehouseMills.pas**

```
unit FRwarehouseMills;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMEwarehouseMills = class(TFRAMEBaseGridEditSOA)
    cxEDTmillWhse: TcxEditRepositoryMaskItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure CDStableBeforePost(DataSet: TDataSet);
  private
    { Private declarations }
    procedure m_FindByCodeMill(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindMill(Sender: TObject; AButtonIndex: Integer);
    function m_ExistsRecord(const pv_dataset: TClientDataSet;
      const pv_fieldName: TStringList): Boolean;
  protected
    { Protected declarations }
    function m_Validate: Boolean;  override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure SetMillsWhseDesc(pv_oldMillWhse, pv_Millwhse: string);
  end;

var
  FRAMEwarehouseMills: TFRAMEwarehouseMills;

implementation

uses
  kneFindDialogSOA, kneUtils, kneTypes, kneFindDialog, kneDialogFactory, 
  kneFGFindUtils, Global,
  //---
  MillServiceUtils, kneFREditSOA;

{$R *.dfm}

constructor TFRAMEwarehouseMills.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'warehouseCode=whse';
  DataPacketName := 'MillWarehouse';              // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'mills';                        // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('mill; millWhse; sapCode; stat');  //millDtime; gpsDtime;
    // Key Fields ..............................................................
    KeyFields:= 'whse;millWhse';
    // Custom Editors ..........................................................
    AddCustomField('mill','cxEDTfind');
    AddCustomField('millWhse','cxEDTmillWhse');
    AddCustomField('stat','cxEDTstat');
  end; //with
  UseColsBestFit := False;
  ColsWidthInGrid := '60;100;90;90';
  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindMill;
end;

procedure TFRAMEwarehouseMills.m_FindMill(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Service: TMillServiceUtils;
  lv_FindOptions: TFindDialogSettings;
  lv_FindResult: integer;
  lv_Find: TFORMkneFindDialog;
begin
  lv_Find := nil;
  try
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    with lv_Find do
    begin

```

#### **FRwarehouseMills.dfm**

```
inherited FRAMEwarehouseMills: TFRAMEwarehouseMills
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxEDTmillWhse: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
    end
  end
end
```
<!-- tabs:end -->

