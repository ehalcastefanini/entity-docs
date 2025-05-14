<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRentityComm` Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRentityComm` unit defines a Delphi frame (`TFRAMEentityComm`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display a grid of entity comments (`EntityComment`) with functionalities such as adding, deleting, and editing comments. It is designed to handle different entity types (e.g., consignee or customer) and provides a user interface for interacting with the data.

This frame is particularly useful in applications where users need to manage comments or notes associated with specific entities, such as customers or consignees, in a structured and user-friendly grid format.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **cxGrid**: A component from DevExpress for displaying and managing tabular data.
- **SOAP Services**: For interacting with external services to fetch or update data.
- **Database Components**: For binding the grid to a database and managing data operations.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **tpComm**: Represents the type of comment (e.g., text or code).
2. **businessUnit**: Represents the business unit associated with the comment.
3. **commText**: The actual comment text.

#### Grid Actions and Their Effects:
1. **Add**: Allows users to add a new comment.
2. **Delete**: Allows users to delete an existing comment.
3. **Edit**: Allows users to modify an existing comment (triggered by value changes in the grid).

---

## 2. Functionality Description:

### User/Software Actions:
1. **Add a Comment**: Users can add a new comment using the "Add" action.
2. **Delete a Comment**: Users can delete a selected comment using the "Delete" action.
3. **Edit a Comment**: Users can edit a comment directly in the grid.

### Main Components:
- **Grid (`cxGrid`)**: Displays the list of comments.
- **ComboBox (`cxCBXbusUnit`)**: Allows selection of a business unit.
- **Action Panel**: Provides buttons for actions like "Add" and "Delete."

### Pseudo-code for Actions and Events:
- `OnClick` event of "Add" button: `if add button clicked then execute ACTaddExecute`.
- `OnEditValueChanged` event of grid: `if grid cell value changed then execute cxDBVtableEditValueChanged`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with specific settings based on the owner component (e.g., `TFORMMconsignee`).
   - Grid settings are configured, including column definitions and read-only fields.
   - Action panel visibility and available actions are set.

2. **User Interactions**:
   - Clicking the "Add" button triggers the `ACTaddExecute` method to add a new comment.
   - Editing a grid cell triggers the `cxDBVtableEditValueChanged` method to handle value changes.

### Functions and Locations:
- **`Create` (Constructor)**: Initializes the frame and sets up grid and action panel settings.
- **`GridSetup`**: Configures the grid, including column definitions and read-only fields.
- **`ACTaddExecute`**: Handles the "Add" action.
- **`cxDBVtableEditValueChanged`**: Handles value changes in the grid.

### Required Data:
- Entity type and code (e.g., `entityCd`, `entityTp`).
- Comments data (e.g., `tpComm`, `businessUnit`, `commText`).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add**: Enabled by default; adds a new comment.
- **Delete**: Enabled when a comment is selected in the grid.
- **Edit**: Automatically triggered when a grid cell value is changed.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- No explicit validations are defined in the code.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame with specific settings based on the owner component.
   - Configures the grid and action panel.

2. **`GridSetup`**:
   - Defines grid columns and read-only fields.

3. **`ACTaddExecute`**:
   - Handles the "Add" action to add a new comment.

4. **`cxDBVtableEditValueChanged`**:
   - Handles value changes in the grid.

---

## 6. API Service Consumption:

- **Service Name**: Not explicitly defined in the code.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Likely to fetch or update comments data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **DevExpress Components**: Used for grid and UI elements.
- **SOAP Components**: Used for interacting with external services.

### Custom Components:
- **`TFRAMEBaseGridEditSOA`**: Base class for the frame.
- **`kneUtils`, `kneTypes`, `Global`**: Utility and type definitions.

---

## 9. Fields and Validations Listing:

### Fields:
1. **tpComm**: Type of comment (string, required).
2. **businessUnit**: Business unit (string, required).
3. **commText**: Comment text (string, required).

### Mapping:
- **tpComm**: Maps to `tpComm` in the database.
- **businessUnit**: Maps to `businessUnit` in the database.
- **commText**: Maps to `commText` in the database.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEentityComm.ACTaddExecute(Sender: TObject);
begin
  // Logic to add a new comment
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Initialization Logic**:
  - The frame is initialized differently based on the owner component (`TFORMMconsignee` or others).
- **Grid Configuration**:
  - Grid columns and read-only fields are defined in the `GridSetup` method.

---

## 12. Conclusion:

The `FRentityComm` unit provides a robust framework for managing entity comments in a grid format. It is highly customizable and integrates well with external services. However, the lack of explicit error handling and field validations may require additional implementation for production use.

---

## 13. Short Summary:

The `FRentityComm` unit defines a grid-based frame for managing entity comments, supporting actions like add, delete, and edit. It integrates with external services and provides a customizable UI for handling comments associated with entities.#### **FRentityComm.pas**

```
// [18-03-2016, #22748]
unit FRentityComm;

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
  TFRAMEentityComm = class(TFRAMEBaseGridEditSOA)
    cxCBXbusUnit: TcxEditRepositoryComboBoxItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  private
    FBusUnitValuesForCbx: String;
    procedure GridSetup;
    procedure SetBusUnitValuesForCbx(const Value: String);
    procedure m_FindByCodeTpComm(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindTpComm(Sender: TObject; AButtonIndex: Integer);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

  published
    property BusUnitValuesForCbx: String read FBusUnitValuesForCbx write SetBusUnitValuesForCbx;
  end;

var
  FRAMEentityComm: TFRAMEentityComm;

implementation

uses
  kneUtils
  , kneTypes
  , Global
  , kneFGFindUtils, kneFindDialog, kneDialogFactory, BaseServiceUtils, LkTpCommServiceUtils    {#23619}
  , CommonLkServiceUtils {#24254};

{$R *.dfm}

const
  mc_GRID_FIELDS = 'tpComm;businessUnit;commText';

{ TFRAMEentityComm }

constructor TFRAMEentityComm.Create(AOwner: TComponent);
begin
  inherited;

  if AOwner.ClassNameIs('TFORMMconsignee') then    // [25-03-2019, #23619]
  begin

    // SET DAS PROPRIEDADES DA FRAME
    MasterKeyFields := 'consCode=entityCd;entityType=entityTp';
    DataPacketName := 'EntityComment';
    PropertyName := 'entityComments';
    FrameType := frtDetail;

  end
  else
  begin

    MasterKeyFields := 'customerCode=entityCd;entityType=entityTp';
    DataPacketName := 'EntityComment';
    PropertyName := 'entityComments';
    FrameType := frtDetail;

  end;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  GridSetup;

end;

procedure TFRAMEentityComm.GridSetup;
begin

  //Definir aqui os settings da cxGrid
  with GridSettings do
  begin

    DefineOrderFields(mc_GRID_FIELDS);

    DefineReadonlyFields('ALL_FIELDS_READ_ONLY');

```

#### **FRentityComm.dfm**

```
inherited FRAMEentityComm: TFRAMEentityComm
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
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
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxCBXbusUnit: TcxEditRepositoryComboBoxItem
    end
  end
end
```
<!-- tabs:end -->

