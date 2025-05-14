<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRdocsCheckRulesCriteria`

## 1. Overview:

### Objective:
The `FRdocsCheckRulesCriteria` unit defines a Delphi frame (`TFRAMEdocsCheckRulesCriteria`) that provides a grid-based interface for managing and editing rule criteria for document checklists. It allows users to add, delete, and modify criteria, with specific configurations for grid behavior and field properties.

### Technologies Used:
- **Delphi VCL Framework**: For UI components and event handling.
- **cxGrid**: A grid component for displaying and editing tabular data.
- **SOAP Services**: For interacting with external services.
- **Database Components**: For managing and displaying data from a database.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **paramId**: Identifier for the parameter (type: string).
2. **paramDesc**: Description of the parameter (type: string).
3. **signField**: Sign field with a dropdown list (`=` or `<>`) (type: combo box).
4. **paramValue**: Value of the parameter (type: string, editable).
5. **paramValueDesc**: Description of the parameter value (type: string).

#### Grid Actions and Their Effects:
1. **Add**: Adds a new rule criterion to the grid.
2. **Delete**: Removes the selected rule criterion from the grid.
3. **Edit**: Allows modification of existing rule criteria.

---

## 2. Functionality Description:

### User/Software Actions:
- Add new rule criteria.
- Edit existing rule criteria.
- Delete selected rule criteria.
- Interact with the grid to modify specific fields.

### Main Components:
1. **Grid (`cxDBG`)**: Displays the rule criteria.
2. **ComboBox (`cxCBXsignField`)**: Provides a dropdown for selecting comparison operators (`=` or `<>`).
3. **Custom Editors**: Configured for specific fields like `signField` and `paramValue`.

### Pseudo-code for Actions and Events:
- **Add Button Click**:  
  `if add button clicked then execute ACTaddExecute procedure`
- **Grid Cell Value Change**:  
  `if grid cell value changed then execute cxDBVtableEditValueChanged procedure`
- **Find Parameter Value**:  
  `if find button clicked then execute m_FindParamValue procedure`

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created using the `Create` constructor.
   - Grid settings are configured in the `GridSetup` method.
   - Action panel and available actions (`ADD`, `DELETE`) are set up.

2. **User Interactions**:
   - Users can add, delete, or edit rule criteria via the grid.
   - Changes in grid cells trigger the `cxDBVtableEditValueChanged` event.

### Functions:
1. **`Create`** (File: `FRdocsCheckRulesCriteria.pas`):
   - Initializes the frame and sets up grid properties.
2. **`GridSetup`** (File: `FRdocsCheckRulesCriteria.pas`):
   - Configures grid fields, hidden fields, and custom editors.
3. **`ACTaddExecute`** (File: `FRdocsCheckRulesCriteria.pas`):
   - Handles the addition of new rule criteria.
4. **`cxDBVtableEditValueChanged`** (File: `FRdocsCheckRulesCriteria.pas`):
   - Handles changes in grid cell values.

### Required Data:
- Rule criteria details such as `paramId`, `paramDesc`, `signField`, `paramValue`, and `paramValueDesc`.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add**:
   - Preconditions: None.
   - Action: Adds a new row to the grid.
2. **Delete**:
   - Preconditions: A row must be selected.
   - Action: Deletes the selected row.
3. **Edit**:
   - Preconditions: A cell must be selected.
   - Action: Allows editing of the selected cell.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **signField**: Dropdown list with fixed options (`=` or `<>`).
- **paramValue**: Uppercase input enforced.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame and sets up metadata and grid properties.
2. **`GridSetup`**:
   - Configures grid fields, hidden fields, and custom editors.
3. **`ACTaddExecute`**:
   - Adds a new rule criterion to the grid.
4. **`cxDBVtableEditValueChanged`**:
   - Handles changes in grid cell values.

---

## 6. API Service Consumption:

- **Service Name**: Not explicitly defined in the code.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Not explicitly defined in the code.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
1. **cxGrid**: For grid display and interaction.
2. **SOAPHTTPClient**: For SOAP service interactions.
3. **DBClient**: For database operations.

### Custom Components:
1. **TFRAMEBaseGridEditSOA**: Base frame class for grid editing.
2. **kneFRGridManager**: Utility for managing grid settings.

---

## 9. Fields and Validations Listing:

1. **paramId**: (type: string, required, not explicitly validated in the code).
2. **paramDesc**: (type: string, required, not explicitly validated in the code).
3. **signField**: (type: combo box, required, options: `=` or `<>`).
4. **paramValue**: (type: string, required, uppercase enforced).
5. **paramValueDesc**: (type: string, optional).

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEdocsCheckRulesCriteria.ACTaddExecute(Sender: TObject);
begin
  // Logic to add a new rule criterion
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **`Create` Constructor**:
  - Sets up metadata and grid properties.
- **`GridSetup` Method**:
  - Configures grid fields and custom editors.

---

## 12. Conclusion:

The `FRdocsCheckRulesCriteria` unit provides a robust framework for managing rule criteria in a grid-based interface. While it offers flexibility in grid configuration and field customization, the lack of explicit error handling and validation logic may require additional implementation for production use.

---

## 13. Short Summary:

The `FRdocsCheckRulesCriteria` unit defines a grid-based interface for managing document checklist rule criteria, supporting add, delete, and edit actions with configurable grid settings and field properties. It integrates with SOAP services and database components for data management.#### **FRdocsCheckRulesCriteria.pas**

```
unit FRdocsCheckRulesCriteria;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils, Grids, DBGrids;

type
  TFRAMEdocsCheckRulesCriteria = class(TFRAMEBaseGridEditSOA)
    cxCBXsignField: TcxEditRepositoryComboBoxItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  private
    mv_AlreadyAdded : string;
    procedure m_FindParams(Sender: TObject; AButtonIndex: Integer);

    procedure GridSetup;
    procedure m_FindParamValue(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindParamValueByCode(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure SetKeyEditing(const EditKey: Boolean); override;

  end;

var
  FRAMEdocsCheckRulesCriteria: TFRAMEdocsCheckRulesCriteria;

implementation

uses
  kneTypes, kneFindDialog, kneDialogFactory, kneUtils, kneFGDBUtils, Global
  , BaseServiceUtils
  , CheckListDocParamsToAddServiceUtils
  , DeliveryTermServiceUtils
  , CustomerMarketServiceUtils
  , ConsigneeMarketServiceUtils
  , RegionServiceUtils
  , PaymentCustomerServiceUtils
  , CustomerWithGenericCriteriaServiceUtils
  , SimpleConsServiceUtils
  , CountryServiceUtils {#23597}
  , MillServiceUtils {#24067}
  ;

const
  mc_GRID_FIELDS = 'paramId;paramDesc;signField;paramValue;paramValueDesc';


{$R *.dfm}

constructor TFRAMEdocsCheckRulesCriteria.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'ruleId';
  DataPacketName := 'CheckListDocCriteria';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'checkListDocCriterias';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  GridSetup;
end;


procedure TFRAMEdocsCheckRulesCriteria.GridSetup;
begin

  with GridSettings do
  begin

    DefineHiddenFields('HIDE_ALL_FIELDS');

    DefineOrderFields(mc_GRID_FIELDS);

//    DefineReadOnlyFields('ALL_FIELDS_READONLY');

    // Custom Editors ..........................................................
   AddCustomField('signField','cxCBXsignField');
   AddCustomField('paramValue','cxEDTfind');

  end; //with
```

#### **FRdocsCheckRulesCriteria.dfm**

```
inherited FRAMEdocsCheckRulesCriteria: TFRAMEdocsCheckRulesCriteria
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      DataController.DataModeController.GridMode = False
      DataController.DataModeController.SmartRefresh = True
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
    object cxCBXsignField: TcxEditRepositoryComboBoxItem
      Properties.Alignment.Horz = taCenter
      Properties.DropDownListStyle = lsEditFixedList
      Properties.Items.Strings = (
        '='
        '<>')
    end
  end
end
```
<!-- tabs:end -->

