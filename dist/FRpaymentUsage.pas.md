<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um componente de interface gráfica chamado `TFRAMEpaymentUsage`, que é uma extensão de um frame base (`TFRAMEBaseGridEditSOA`). Ele é utilizado para exibir e gerenciar dados relacionados ao uso de pagamentos em um formato de grade (grid). Este componente permite a visualização, adição e exclusão de registros, além de configurar propriedades específicas da grade, como campos visíveis, ordem de exibição e editores personalizados.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes VCL:** Incluindo `TcxGrid`, `TcxEditRepositoryImageComboBoxItem`, e outros componentes visuais para a interface.
  - **SOAP:** Para comunicação com serviços externos.
  - **Banco de Dados:** Manipulação de dados através de `TField` e `CDStable`.

* **Forma do Componente:**
  - **Exibição em Grade (Grid):**
    - **Colunas da Grade e Tipos:**
      - `entityType` (Tipo: String, com editor personalizado `cxEDTicboEntityType`).
      - `stat` (Tipo: String, com editor personalizado `cxEDTstat`).
    - **Ações da Grade e Efeitos:**
      - Adicionar (`ADD`): Insere um novo registro com o status padrão "ACTIVE".
      - Excluir (`DELETE`): Remove registros selecionados.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar um novo registro.
  - Excluir registros existentes.
  - Configurar e exibir dados em uma grade com colunas personalizadas.

* **Componentes Principais:**
  - `TFRAMEpaymentUsage`: Frame principal que gerencia a exibição e manipulação dos dados.
  - `cxEDTicboEntityType`: ComboBox para seleção de valores do campo `entityType`.
  - `GridSettings`: Configurações da grade, como campos ocultos, ordem de exibição e editores personalizados.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar": `if botão "Adicionar" clicado then insere novo registro com status "ACTIVE"`.
  - Evento `OnShow` do frame: `if frame carregado then preenche valores da ComboBox com dados do campo "entityType"`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do frame (`Create`):
     - Configurações iniciais da grade, como campos ocultos, ordem e editores personalizados.
     - Define ações disponíveis (`ADD` e `DELETE`).
  2. Exibição dos dados (`ShowData`):
     - Preenche a ComboBox `cxEDTicboEntityType` com valores possíveis do campo `entityType`.
  3. Interação do Usuário:
     - Ao clicar no botão "Adicionar", um novo registro é criado com o status "ACTIVE".

* **Dados Necessários:**
  - Valores possíveis para o campo `entityType` (obtidos da metadata).
  - Dados existentes na tabela `CDStable`.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre. Insere um registro com o status padrão "ACTIVE".
  - **Excluir:** Disponível apenas se um registro estiver selecionado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Campo `stat`: Valor padrão "ACTIVE".

* **Validações e Condições dos Campos:**
  - Campo `entityType`: Preenchido com valores possíveis obtidos da metadata.
  - Campo `stat`: Valor padrão "ACTIVE".

---

## 5. Funções Principais:

* **`Create`:**
  - Configura as propriedades iniciais do frame e da grade.
* **`ShowData`:**
  - Preenche a ComboBox `cxEDTicboEntityType` com valores possíveis do campo `entityType`.
* **`ACTaddExecute`:**
  - Insere um novo registro na tabela com o status "ACTIVE".

---

## 6. Consumo de Serviços API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `cxGrid`, `cxEditRepositoryItems`: Para componentes visuais e editores personalizados.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Frame base herdado.
  - `TkneDB`: Utilizado para obter valores possíveis de campos.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `entityType` (Tipo: String, obrigatório, valores definidos pela metadata).
  - `stat` (Tipo: String, obrigatório, valor padrão "ACTIVE").

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `entityType`: Coluna correspondente no banco de dados.
  - `stat`: Coluna correspondente no banco de dados.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**
  ```delphi
  FRAMEpaymentUsage := TFRAMEpaymentUsage.Create(Self);
  FRAMEpaymentUsage.ShowData;
  ```

* **HTML Representando a Grade:**
  ```html
  <table style="width: 100%; border: 1px solid black;">
    <thead>
      <tr>
        <th style="width: 100px;">Entity Type</th>
        <th style="width: 100px;">Status</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Type 1</td>
        <td>ACTIVE</td>
      </tr>
      <tr>
        <td>Type 2</td>
        <td>INACTIVE</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração inicial do frame e da grade no método `Create`.
* Preenchimento da ComboBox `cxEDTicboEntityType` no método `ShowData`.

---

## 12. Conclusão:

O código implementa um frame funcional para gerenciar dados de uso de pagamentos em uma grade. Ele é bem estruturado, com configurações claras para a grade e suporte a ações básicas como adicionar e excluir registros. No entanto, faltam mensagens de erro e validações mais robustas.

---

## 13. Resumo Curto:

O `TFRAMEpaymentUsage` é um frame para exibição e manipulação de dados de uso de pagamentos em uma grade, com suporte a ações de adicionar e excluir registros, além de configurações personalizadas para colunas e editores.#### **FRpaymentUsage.pas**

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

