<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRpaymentUsage` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRpaymentUsage` code unit defines a grid-based user interface for managing payment usage data. It provides functionalities to display, add, and manage payment usage records in a structured grid format. The main objective is to allow users to interact with payment usage data efficiently, including adding new records and configuring grid settings dynamically.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL framework for UI components.
- **cxGrid**: A grid component from DevExpress for displaying and managing tabular data.
- **SOAP**: Used for communication with external services.
- **Database Components**: Includes `DBClient` and `TField` for database interaction.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **entityType**: Custom editor (`cxEDTicboEntityType`).
2. **stat**: Custom editor (`cxEDTstat`).
3. **paymentCode**: Hidden field.

#### Grid Actions and Their Effects:
1. **ADD**: Adds a new record to the grid and sets the `stat` field to "ACTIVE".
2. **DELETE**: Deletes the selected record from the grid.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Display Data**: Loads and displays payment usage data in the grid.
2. **Add Record**: Adds a new record with default values.
3. **Configure Grid**: Dynamically configures grid settings such as hidden fields, column order, and custom editors.

### Main Components:
- **Grid Settings**: Configures the grid's appearance and behavior.
- **Custom Editors**: Provides specific editors for certain fields (e.g., `cxEDTicboEntityType`).
- **Action Handlers**: Handles user actions like adding records.

### Pseudo-code for Actions and Events:
- **OnClick event of "Add" button**:  
  `if ADD button clicked then execute ACTaddExecute`
- **OnShowData event**:  
  `if ShowData triggered then populate grid and fill ComboBox with metadata`

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**: The `TFRAMEpaymentUsage` constructor initializes the grid settings, hidden fields, column order, and custom editors.
2. **Data Display**: The `ShowData` method populates the grid and fills the ComboBox with metadata values.
3. **User Interaction**: Users can add or delete records using the available actions.

### Functions and File Locations:
1. **Constructor** (`Create` in `FRpaymentUsage`):
   - Initializes grid settings and available actions.
2. **ShowData** (`ShowData` in `FRpaymentUsage`):
   - Populates the grid and ComboBox with metadata.
3. **Add Record** (`ACTaddExecute` in `FRpaymentUsage`):
   - Adds a new record and sets default values.

### Required User Data:
- **entityType**: Selected from a ComboBox.
- **stat**: Automatically set to "ACTIVE" when adding a record.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Record**:
   - Preconditions: None.
   - Action: Adds a new record with default values.
2. **Delete Record**:
   - Preconditions: A record must be selected.
   - Action: Deletes the selected record.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- **stat**: Default value is "ACTIVE" when adding a new record.

### Field Validation and Conditions:
- **entityType**: Populated with metadata values.
- **stat**: Automatically set to "ACTIVE".

---

## 5. Main Functions:

1. **Constructor (`Create`)**:
   - Initializes grid settings, hidden fields, column order, and custom editors.
2. **ShowData**:
   - Populates the grid and ComboBox with metadata values.
3. **ACTaddExecute**:
   - Adds a new record and sets the `stat` field to "ACTIVE".

---

## 6. API Service Consumption:

- **Service Name**: Not explicitly defined in the code.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Metadata for populating the ComboBox.
- **Data Received**: Possible values for the `entityType` field.
- **Purpose**: Populate the ComboBox with metadata values.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **entityType**: Populated dynamically based on metadata values.
- **Conditions**: The ComboBox is filled only when `ShowData` is executed.

---

## 8. Dependencies:

### External Libraries:
1. **DevExpress Components**:
   - `cxGrid`: For grid display and management.
   - `cxEditRepositoryItems`: For custom editors.
2. **SOAP Components**:
   - `SOAPHTTPClient`: For external service communication.

### Custom Components:
- **TFRAMEBaseGridEditSOA**: Base class for grid-based forms.

---

## 9. Fields and Validations Listing:

1. **entityType**:
   - Type: String.
   - Required: Yes.
   - Validation: Populated with metadata values.
2. **stat**:
   - Type: String.
   - Default: "ACTIVE".
   - Validation: None explicitly defined.
3. **paymentCode**:
   - Type: String.
   - Hidden field.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Grid Settings] --> [ShowData] --> [Populate Grid and ComboBox] --> [User Interaction: Add/Delete Record] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Grid: Add Record
Grid --> Database: Insert Record
Database --> Grid: Update Display
```

### Code Snippets:
```delphi
procedure TFRAMEpaymentUsage.ACTaddExecute(Sender: TObject);
begin
  inherited;
  SetForEdition(CDStable);
  CDStable.FieldByName('stat').AsString := 'ACTIVE';
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

1. **Grid Settings Initialization**:
   - Configures hidden fields, column order, and custom editors.
2. **ComboBox Population**:
   - Populates the `entityType` ComboBox with metadata values.

---

## 12. Conclusion:

The `FRpaymentUsage` code unit provides a robust framework for managing payment usage data in a grid format. It dynamically configures grid settings and supports adding new records with default values. However, error handling and API service details are not explicitly defined, which could limit its robustness in certain scenarios.

---

## 13. Short Summary:

The `FRpaymentUsage` unit manages payment usage data in a grid interface, allowing users to add and configure records dynamically. It leverages metadata for field population and supports custom editors for enhanced usability.#### **FRpaymentUsage.pas**

```
unit FRpaymentUsage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  cxDBEdit;

type
  TFRAMEpaymentUsage = class(TFRAMEBaseGridEditSOA)
    cxEDTicboEntityType: TcxEditRepositoryImageComboBoxItem;
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure ShowData; override;
  end;

var
  FRAMEpaymentUsage: TFRAMEpaymentUsage;

implementation

uses
  Global, KneTypes, kneUtils;

{$R *.dfm}

{ TFRAMEpaymentUsage }

constructor TFRAMEpaymentUsage.Create(AOwner: TComponent);
begin
  inherited;

	// SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'paymentCode';
  DataPacketName := 'PaymentUsage';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'usages';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin

    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');

    // Ordem Campos ............................................................
    DefineOrderFields('entityType;stat');

    // Key Fields ..............................................................
    KeyFields:= 'paymentCode;entityType';

    // Tamanho Campos ............................................................
    ColsWidthInGrid := '100; 100; 300';

    // Custom Editors ..........................................................
    AddCustomField('entityType', 'cxEDTicboEntityType');
    AddCustomField('stat', 'cxEDTstat');

  end; //with
end;

procedure TFRAMEpaymentUsage.ShowData;
var
  lv_Field: TField;
  lv_PossibleValues: string;
begin
  inherited;

  // Preenche a ComboBox cxEDTicboEntityType com os valores vindos da metadata para o campo "entityType"
  lv_Field := CDStable.FieldByName('entityType');
  lv_PossibleValues := TkneDB.GetFieldPossibleValues(lv_Field);
  FillImageComboBoxValues(cxEDTicboEntityType, lv_PossibleValues);
end;

procedure TFRAMEpaymentUsage.ACTaddExecute(Sender: TObject);
begin
  inherited;

  SetForEdition(CDStable);
  CDStable.FieldByName('stat').AsString := 'ACTIVE';
end;

end.
```

#### **FRpaymentUsage.dfm**

```
inherited FRAMEpaymentUsage: TFRAMEpaymentUsage
  ParentFont = True
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
    object cxEDTicboEntityType: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <>
    end
  end
end
```
<!-- tabs:end -->

