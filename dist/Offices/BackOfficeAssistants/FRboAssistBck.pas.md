<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRboAssistBck` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRboAssistBck` code unit defines a specialized frame (`TFRAMEboAssistBck`) for managing and editing a grid-based interface related to "BoAssistBck" data. It provides functionalities for adding, deleting, and validating records, as well as customizing grid behavior and appearance. This frame is part of a larger system that handles data management and user interaction with a database.

### Technologies Used:
- **Delphi**: Object Pascal programming language and VCL framework.
- **DevExpress Components**: For grid and UI elements (`TcxGrid`, `TcxEditRepositoryDateItem`, etc.).
- **SOAP Services**: For backend communication (`SOAPHTTPClient`, `Rio`).
- **Database Components**: For data handling (`DB`, `DBClient`).

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. `boAssistBck` - Custom field with a custom editor (`cxEDTfind`).
2. `name` - Text field.
3. `dateIni` - Date field.
4. `dateFim` - Date field.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new record to the grid.
2. **Delete**: Deletes the selected record from the grid.
3. **Edit**: Allows editing of existing records.
4. **Validation**: Validates records before saving.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new record to the grid.
2. Delete an existing record.
3. Edit and validate records.
4. Search for records using a custom find dialog.

### Main Components:
- **Grid (`TcxGrid`)**: Displays the data in a tabular format.
- **Custom Editor (`cxEDTfind`)**: Provides a custom search functionality.
- **Action Panel**: Contains buttons for actions like Add and Delete.
- **Validation Logic**: Ensures data integrity before saving.

### Pseudo-code for Actions and Events:
1. **OnClick event of Add button**:  
   `if Add button clicked then execute ACTaddExecute function`.
2. **OnEditValueChanged event of grid**:  
   `if grid cell value changed then execute cxDBVtableEditValueChanged function`.
3. **OnButtonClick event of custom editor**:  
   `if custom editor button clicked then execute m_FindBoAssistBck function`.
4. **OnValidateRecord event**:  
   `if record is being saved then execute m_OnValidateRecord function`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:  
   - The `TFRAMEboAssistBck` frame is created and initialized in the `Create` constructor.
   - Grid settings, action panel visibility, and available actions are configured.
   - Custom field editors and validation logic are set up.

2. **User Interactions**:  
   - Users interact with the grid to add, delete, or edit records.
   - Clicking the custom editor button opens a find dialog for searching records.
   - Changes in grid values trigger validation and update logic.

### Functions and File Locations:
1. **`Create` Constructor** (in `FRboAssistBck`): Initializes the frame and its components.
2. **`m_FindBoAssistBck`** (in `FRboAssistBck`): Handles the custom search functionality.
3. **`m_OnValidateRecord`** (in `FRboAssistBck`): Validates records before saving.
4. **`ACTaddExecute`** (in `FRboAssistBck`): Adds a new record to the grid.

### Required User Data:
- `boAssistBck` (custom field).
- `name` (text field).
- `dateIni` and `dateFim` (date fields).

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add**: Enabled at all times.
2. **Delete**: Enabled only when a record is selected.
3. **Edit**: Enabled when a record is selected.
4. **Validation**: Triggered automatically when saving a record.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Required field not completed" if a required field is empty.
- "Invalid date" if a date field contains an invalid value.
- "Value out of range" if a field value exceeds allowed limits.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- `boAssistBck`: Custom validation logic.
- `name`: Should not be empty.
- `dateIni` and `dateFim`: Must be valid dates.

---

## 5. Main Functions:

1. **`Create` Constructor**: Initializes the frame, grid settings, and action panel.
2. **`m_FindBoAssistBck`**: Opens a find dialog for searching records.
3. **`m_OnValidateRecord`**: Validates records before saving.
4. **`ACTaddExecute`**: Adds a new record to the grid.

---

## 6. API Service Consumption:

- **Service Name**: `BoAssistServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Metadata and record details.
- **Data Received**: Updated record details.
- **Purpose**: Manage "BoAssistBck" data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Custom Editor (`cxEDTfind`)**: Visible and functional only when the user interacts with the corresponding field.

---

## 8. Dependencies:

### External Libraries:
- **DevExpress Components**: For grid and UI elements.
- **SOAPHTTPClient**: For backend communication.

### Custom Components:
- **`TFRAMEBaseGridEditSOA`**: Base class for the frame.
- **`TFORMkneFindDialog`**: Custom find dialog.

---

## 9. Fields and Validations Listing:

1. **boAssistBck**: Custom field, required.
2. **name**: Text field, required.
3. **dateIni**: Date field, required.
4. **dateFim**: Date field, required.

Mapping of displayed values and database columns is not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEboAssistBck.m_FindBoAssistBck(Sender: TObject; AButtonIndex: Integer);
begin
  // Custom search logic
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Settings**: Configured in the `Create` constructor.
- **Custom Editor**: `cxEDTfind` is linked to `m_FindBoAssistBck`.
- **Validation Logic**: Defined in `m_OnValidateRecord`.

---

## 12. Conclusion:

The `FRboAssistBck` code unit provides a robust framework for managing grid-based data related to "BoAssistBck." It includes features for adding, deleting, and validating records, as well as customizing grid behavior. However, the code lacks explicit error handling and API endpoint definitions.

---

## 13. Short Summary:

The `FRboAssistBck` unit defines a grid-based interface for managing "BoAssistBck" data, with features for adding, deleting, and validating records. It uses DevExpress components and SOAP services for UI and backend communication.#### **FRboAssistBck.pas**

```
unit FRboAssistBck;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEboAssistBck = class(TFRAMEBaseGridEditSOA)
    cxEDTRDateItem1: TcxEditRepositoryDateItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindBoAssistBck(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindBoAssistBckByCode(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_OnValidateRecord(Dataset: TDataSet);
    
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure SetKeyEditing(const EditKey: Boolean); override;

  end;

var
  FRAMEboAssistBck: TFRAMEboAssistBck;

implementation

uses
  kneTypes, kneFindDialog, kneDialogFactory, kneUtils, Global
  , BaseServiceUtils
  , BoAssistServiceUtils
  , kneConfigObjects;

{$R *.dfm}

constructor TFRAMEboAssistBck.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'boAssist=boAssistCd';
  DataPacketName := 'BoAssistBck';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'boAssistBcks';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin

    HiddenFields.Add('HIDE_ALL_FIELDS');

    DefineOrderFields('boAssistBck;name;dateIni;dateFim');

    // Key Fields ..............................................................
//    KeyFields:= '';
    // Custom Editors ..........................................................
    AddCustomField('boAssistBck','cxEDTfind');

  end; //with


  UseColsBestFit := False; // Para que n�o execute o ApplyBestFit e aceite o tamanho na ColsWidthInGrid
  ColsWidthInGrid := '170;170;170';


  cxEDTfind.Properties.OnButtonClick := m_FindBoAssistBck;

  OnValidateRecord := m_OnValidateRecord;

end;

procedure TFRAMEboAssistBck.m_FindBoAssistBck(
  Sender: TObject; AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
  lv_FieldName : string;
  lv_FindResult, lv_i : Integer;
begin
  lv_Find := nil;

  CDStable.DisableControls;

```

#### **FRboAssistBck.dfm**

```
inherited FRAMEboAssistBck: TFRAMEboAssistBck
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      DataController.DataModeController.GridMode = False
    end
  end
  inherited cxSTLR: TcxStyleRepository
    inherited cxSTLReadOnly: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLDefault: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLInactive: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLgroupBox: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLheader: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLselection: TcxStyle
      Font.Name = 'Verdana'
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTRDateItem1: TcxEditRepositoryDateItem
    end
  end
end
```
<!-- tabs:end -->

