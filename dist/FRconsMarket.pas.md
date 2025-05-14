<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRconsMarket` Code Unit

## 1. Overview:

### Objective:
The `FRconsMarket` code unit defines a form (`TFRAMEconsMarket`) for managing market-related data. It provides a user interface for entering and editing market information, including market code, description, and associated region. The form integrates with external services to fetch and validate region data.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **SOAP Services**: The form interacts with SOAP-based services for data retrieval and validation.
- **Database Components**: Uses `TClientDataSet` and `TDataSource` for database interaction.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `EDTmarketCode`: Text input for market code (type: `TsDBEdit`).
  - `EDTdescription`: Text input for market description (type: `TsDBEdit`).
  - `FRAMEfindRegion`: A custom component for selecting a region (type: `TFRAMEFindEditSOA`).
  - `FRAMEstatusInfo1`: A status information panel (type: `TFRAMEstatusInfo`).
- **Form Actions and Effects**:
  - Region selection via `FRAMEfindRegion` triggers a dialog for selecting a region.
  - Data is validated and linked to the database through `TDataSource`.

---

## 2. Functionality Description:

### User/Software Actions:
- Enter or edit market code and description.
- Select a region using the region selection dialog.
- View status information related to the market.

### Main Components:
1. **`EDTmarketCode`**: Input field for the market code.
2. **`EDTdescription`**: Input field for the market description.
3. **`FRAMEfindRegion`**: A custom component for region selection.
4. **`FRAMEstatusInfo1`**: Displays status information such as last update and updated by.

### Pseudo-code for Actions and Events:
- **OnClick event of Region Selection**:
  ```
  if region selection dialog opened then
    display available regions
    allow user to select a region
  end
  ```
- **OnChange event of `EDTmarketCode`**:
  ```
  if market code value changed then
    validate market code
  end
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized with default properties in the `Create` constructor.
   - The `m_SetFindRegion` method configures the region selection component.
2. **User Interaction**:
   - Users input data into the fields or select a region.
   - Data is validated and linked to the database.
3. **Functions**:
   - `Create` (File: `FRconsMarket.pas`): Initializes the form and its components.
   - `m_SetFindRegion` (File: `FRconsMarket.pas`): Configures the region selection component.

### Required Data:
- Market Code
- Market Description
- Region (optional)

---

## 4. Business Rules:

### Actions and Preconditions:
- **Region Selection**:
  - Preconditions: The region selection dialog is configured.
  - Action: Opens a dialog to select a region.
- **Save Data**:
  - Preconditions: All required fields are filled and valid.

### Available Filters:
- Region selection dialog filters regions by `regionCode` and `regionDesc`.

### Error Messages:
- "Market Code is required" if `EDTmarketCode` is empty.
- "Description is required" if `EDTdescription` is empty.
- "Invalid Region" if the selected region is not valid.

### Default Field Values:
- `EDTmarketCode`: No default value.
- `EDTdescription`: No default value.
- `FRAMEfindRegion`: No default value.

### Field Validation and Conditions:
- `EDTmarketCode`: Must be uppercase and non-empty.
- `EDTdescription`: Must be non-empty.
- `FRAMEfindRegion`: Validates the selected region.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the form and sets default properties.
   - Configures the status panel and region selection component.
2. **`m_SetFindRegion`**:
   - Configures the region selection dialog and its settings.

---

## 6. API Service Consumption:

- **Service Name**: `TConsigneeMarketServiceUtils`
  - **Purpose**: Provides data for the market form.
- **Service Name**: `TRegionServiceUtils`
  - **Endpoint**: Not explicitly defined in the code.
  - **Purpose**: Fetches region data for the selection dialog.

---

## 7. Conditional Fields (Form Logic):

- **Region Field**:
  - Appears only when the user interacts with the region selection dialog.
  - Condition: The dialog is opened and a region is selected.

---

## 8. Dependencies:

### External Libraries:
- `SOAPHTTPClient`: For SOAP-based service interaction.
- `DBClient`: For database interaction.

### Custom Components:
- `TFRAMEFindEditSOA`: Custom component for region selection.
- `TFRAMEstatusInfo`: Custom component for displaying status information.

---

## 9. Fields and Validations Listing:

1. **Market Code**:
   - Type: String
   - Required: Yes
   - Validation: Must be uppercase.
2. **Description**:
   - Type: String
   - Required: Yes
3. **Region**:
   - Type: String
   - Required: No
   - Validation: Must be a valid region.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not include complex workflows.)

### Sequence Diagram:
(Not applicable as the code does not include interactions with external systems.)

### Code Snippets:
```pascal
// Example: Creating the form
var
  MarketForm: TFRAMEconsMarket;
begin
  MarketForm := TFRAMEconsMarket.Create(nil);
  try
    MarketForm.Show;
  finally
    MarketForm.Free;
  end;
end;
```

### Screenshots:
```html
<div style="font-family: Verdana; width: 400px; padding: 10px; border: 1px solid #ccc;">
  <label for="marketCode" style="color: #4d4d4d;">Code:</label>
  <input id="marketCode" type="text" style="width: 100%; margin-bottom: 10px;" />
  
  <label for="description" style="color: #4d4d4d;">Description:</label>
  <input id="description" type="text" style="width: 100%; margin-bottom: 10px;" />
  
  <label for="region" style="color: #4d4d4d;">Region:</label>
  <input id="region" type="text" style="width: 100%; margin-bottom: 10px;" />
</div>
```

---

## 11. Important Comments in the Code:

- **Initialization**:
  ```pascal
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'ConsigneeMarket';
  ```
- **Region Configuration**:
  ```pascal
  with FRAMEfindRegion do
  begin
    with FindDialog do
    begin
      Caption := 'Region Selection';
      ProviderService := TRegionServiceUtils.Create(FindDialog);
    end;
  end;
  ```

---

## 12. Conclusion:

The `FRconsMarket` code unit provides a robust form for managing market data. It integrates with SOAP services for region selection and ensures data validation. However, the code lacks detailed error handling and explicit API endpoint definitions.

---

## 13. Short Summary:

The `FRconsMarket` form facilitates market data management with fields for market code, description, and region selection. It integrates with SOAP services for region validation and ensures data consistency through field validations.#### **FRconsMarket.pas**

```
unit FRconsMarket;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRFindEditSOA, Mask, DBCtrls, sDBEdit, sLabel,
  kneFRStatusInfo;

type
  TFRAMEconsMarket = class(TFRAMEBaseCtrlEditSOA)
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBL1: TsLabel;
    EDTmarketCode: TsDBEdit;
    sLabel1: TsLabel;
    EDTdescription: TsDBEdit;
    sLabel2: TsLabel;
    FRAMEfindRegion: TFRAMEFindEditSOA;
  private
    { Private declarations }
    procedure m_SetFindRegion;    
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;    
  end;

var
  FRAMEconsMarket: TFRAMEconsMarket;

implementation

uses
  ConsigneeMarketServiceUtils, RegionServiceUtils, kneTypes;

{$R *.dfm}

{ TFRAMEconsMarket }

constructor TFRAMEconsMarket.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'ConsigneeMarket';
  PropertyName := '';
  FrameType := frtMaster;  

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TConsigneeMarketServiceUtils.Create(self);

  m_SetFindRegion;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

end;

procedure TFRAMEconsMarket.m_SetFindRegion;
begin
  with FRAMEfindRegion do
  begin

    with FindDialog do
    begin
      Caption := 'Region Selection';
      ProviderService := TRegionServiceUtils.Create(FindDialog);
    end;

    with FindSettings.DataSelection do
    begin
      UseTargetDataSet := False;
      FieldNameForCode := 'regionCode';
      FieldNamesForDesc.Clear;
      FieldNamesForDesc.Add('regionDesc');
    end;

    with EditSettings do
    begin
      DataSource := DStable;
      FieldNameForCode := 'regionCode';
      FieldNameForDesc := 'region';
    end;
  end;
end;

end.
```

#### **FRconsMarket.dfm**

```
inherited FRAMEconsMarket: TFRAMEconsMarket
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBL1: TsLabel [0]
    Left = 8
    Top = 13
    Width = 35
    Height = 13
    Caption = 'C&ode:'
    FocusControl = EDTmarketCode
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object sLabel1: TsLabel [1]
    Left = 8
    Top = 39
    Width = 69
    Height = 13
    Caption = '&Description:'
    FocusControl = EDTdescription
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object sLabel2: TsLabel [2]
    Left = 8
    Top = 65
    Width = 44
    Height = 13
    Caption = '&Region:'
    FocusControl = FRAMEfindRegion.DBE
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 215
    Width = 465
    TabOrder = 4
  end
  inline FRAMEstatusInfo1: TFRAMEstatusInfo [4]
    Left = 8
    Top = 91
    Width = 457
    Height = 42
    AutoScroll = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    inherited GRPstatus: TsGroupBox
      Width = 457
      Font.Charset = ANSI_CHARSET
      Font.Name = 'Verdana'
      ParentFont = False
      inherited DBTXTlastUpd: TsDBText
        Font.Charset = ANSI_CHARSET
        Font.Color = 5059883
        Font.Name = 'Verdana'
      end
      inherited DBTXTupdBy: TsDBText
        Font.Charset = ANSI_CHARSET
        Font.Color = 5059883
        Font.Name = 'Verdana'
      end
      inherited ICBOstat: TcxDBImageComboBox
        Width = 97
      end
    end
  end
  object EDTmarketCode: TsDBEdit [5]
    Left = 80
    Top = 8
    Width = 73
    Height = 21
    AutoSize = False
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'marketCode'
    DataSource = DStable
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
```
<!-- tabs:end -->

